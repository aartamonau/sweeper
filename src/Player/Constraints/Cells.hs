{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Player.Constraints.Cells
       (
         Formula
       , Cell
       , CellType (IOCell, ComputedCell)
       , CellValue(merge, isFinal)
       , v
       , computedCell
       , addFormula
       , ioCell
       , getValue
       , setValue
       , modifyValue
       )
       where

import Control.Monad (when)
import Control.Monad.ST (ST, RealWorld)

import Data.List ((\\), nub, delete)
import Data.STRef.Strict (STRef,
                          newSTRef, readSTRef, writeSTRef, modifySTRef')
import Data.Typeable (Typeable, cast)

import Data.Proxy (Proxy(Proxy))

data Dep = forall k a. (Typeable k, CellValue a) => Dep (Cell k a)

instance Eq Dep where
  Dep x == Dep y =
    case (cast y) of
      Just y' -> y' == x
      Nothing -> False

data Formula a = Formula { deps    :: [Dep]
                         , compute :: ST RealWorld a
                         }
               deriving Functor

instance Applicative Formula where
  pure x = Formula { compute = pure x
                   , deps    = []
                   }

  f <*> x = Formula { compute = fcompute <*> xcompute
                    , deps    = nub (fdeps ++ xdeps)
                    }
    where Formula { compute = fcompute, deps = fdeps } = f
          Formula { compute = xcompute, deps = xdeps } = x

class (Typeable a, Eq a) => CellValue a where
  merge   :: a -> a -> a
  merge x y | x == y    = x
            | otherwise = error "default merge assumes equal values"

  isFinal :: a -> Bool
  isFinal = const False

instance CellValue Int

data Value a = Decided a
             | Undecided a
             deriving Eq

data CellType = ComputedCell | IOCell

data CellState a = CellState { rdeps   :: [Dep]
                             , value   :: Value a
                             , formula :: Formula a
                             }

data Cell :: CellType -> * -> * where
  MkCell :: Typeable kind
            => Proxy kind -> STRef RealWorld (CellState a) -> Cell kind a
  deriving Typeable

instance Eq (Cell k a) where
  MkCell _ x == MkCell _ y = x == y

makeValue :: CellValue a => a -> Value a
makeValue x | isFinal x = Decided x
            | otherwise = Undecided x

extractValue :: Value a -> a
extractValue (Undecided x) = x
extractValue (Decided x)   = x

makeFormula :: [Dep] -> ST RealWorld a -> Formula a
makeFormula = Formula

v :: CellValue a => Cell k a -> Formula a
v cell@(MkCell _ ref) = makeFormula [Dep cell] (get <$> readSTRef ref)
  where get = extractValue . value

computedCell :: CellValue a => Formula a -> ST RealWorld (Cell 'ComputedCell a)
computedCell f =
  do v <- compute f

     let value = makeValue v
     let state = CellState { rdeps   = []
                           , value   = value
                           , formula = f }

     ref <- newSTRef state
     let cell = MkCell Proxy ref
     insertAsRDep cell (deps f)
     return cell

  where insertAsRDep cell = updateRDepsWith insert
          where dep          = Dep cell
                insert rdeps = dep:rdeps

addFormula :: CellValue a => Cell 'ComputedCell a -> Formula a -> ST RealWorld ()
addFormula cell@(MkCell _ ref) newFormula =
  do state@CellState{rdeps, value, formula} <- readSTRef ref

     assertUndecided value "attempting to add formula to a decided cell"

     let formula' = merge <$> formula <*> newFormula

     v <- compute formula'

     let newValue = makeValue v
     let newState = state { formula = formula'
                          , value   = newValue
                          }
     writeSTRef ref newState

     updateRDeps newValue (deps formula) (deps formula')
     mapM_ invalidateDep rdeps

  where updateRDeps (Decided _) oldDeps _         = updateRDepsWith remove oldDeps
        updateRDeps (Undecided _) oldDeps newDeps =
          updateRDepsWith insert (newDeps \\ oldDeps)

        dep = Dep cell

        remove = delete dep
        insert = (dep:)

updateRDepsWith :: ([Dep] -> [Dep]) -> [Dep] -> ST RealWorld ()
updateRDepsWith op deps = mapM_ updateOneDep deps
  where updateOneDep (Dep (MkCell _ depCell)) = modifySTRef' depCell updateRDeps
          where updateRDeps state@(CellState {rdeps}) =
                  state { rdeps = op rdeps }

ioCell :: a -> ST RealWorld (Cell 'IOCell a)
ioCell v = MkCell Proxy <$> newSTRef state
  where state = CellState {value = Undecided v, rdeps = [], formula = pure v}

getValue :: Cell k a -> ST RealWorld a
getValue (MkCell _ cell) = readSTRef cell >>= return . extractValue . value

setValue :: CellValue a => Cell 'IOCell a -> a -> ST RealWorld ()
setValue = setValueInternal

modifyValue :: CellValue a => Cell 'IOCell a -> (a -> a) -> ST RealWorld ()
modifyValue cell f = getValue cell >>= setValue cell . f

setValueInternal :: CellValue a => Cell k a -> a -> ST RealWorld ()
setValueInternal cell@(MkCell _ ref) v =
  do state@CellState {value} <- readSTRef ref

     assertUndecided value "attempting to overwrite decided value"
     when (newValue /= value) $ set state

  where newValue = makeValue v

        set state =
          do writeSTRef ref (state {value = newValue})
             updateRDeps newValue (deps $ formula state)
             mapM_ invalidateDep (rdeps state)

        updateRDeps (Decided _) deps = updateRDepsWith (delete dep) deps
        -- a formula can't change its dependencies, so we can simply do
        -- nothing here
        updateRDeps (Undecided _) _  = return ()

        dep = Dep cell

invalidateDep :: Dep -> ST RealWorld ()
invalidateDep (Dep cell) = invalidate cell

invalidate :: CellValue a => Cell k a -> ST RealWorld ()
invalidate cell@(MkCell _ ref) =
  do CellState {formula} <- readSTRef ref

     newValue <- compute formula
     setValueInternal cell newValue

assertUndecided :: Value a -> String -> ST s ()
assertUndecided (Decided _) msg = error msg
assertUndecided _ _             = return ()
