{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Player.Smart.Cells
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

import Data.List (nub, delete)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Typeable (Typeable, cast)

import Data.Proxy (Proxy(Proxy))

data Dep = forall k a. (Typeable k, CellValue a) => Dep (Cell k a)

instance Eq Dep where
  Dep x == Dep y =
    case (cast y) of
     Just y' -> y' == x
     Nothing -> False

data Formula a = Formula { deps    :: [Dep]
                         , compute :: IO a
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
  MkCell :: Typeable kind => Proxy kind -> IORef (CellState a) -> Cell kind a

instance Eq (Cell k a) where
  MkCell _ x == MkCell _ y = x == y

makeValue :: CellValue a => a -> Value a
makeValue x | isFinal x = Decided x
            | otherwise = Undecided x

extractValue :: Value a -> a
extractValue (Undecided x) = x
extractValue (Decided x)   = x

makeFormula :: [Dep] -> IO a -> Formula a
makeFormula = Formula

v :: CellValue a => Cell k a -> Formula a
v cell@(MkCell _ ref) = makeFormula [Dep cell] (get <$> readIORef ref)
  where get = extractValue . value

computedCell :: CellValue a => Formula a -> IO (Cell 'ComputedCell a)
computedCell f =
  do v <- compute f

     let value = makeValue v
     let state = CellState { rdeps   = []
                           , value   = value
                           , formula = f }

     ref <- newIORef state
     let cell = MkCell Proxy ref
     insertAsRDep cell (deps f)
     return cell

  where insertAsRDep cell = updateRDepsWith insert
          where dep          = Dep cell
                insert rdeps = dep:rdeps

addFormula :: CellValue a => Cell 'ComputedCell a -> Formula a -> IO ()
addFormula cell@(MkCell _ ref) newFormula =
  do state@CellState{..} <- readIORef ref

     assertUndecided value "attempting to add formula to a decided cell"

     let formula' = merge <$> formula <*> newFormula

     v <- compute formula'

     let newValue = makeValue v
     let newState = state { formula = formula'
                          , value   = newValue
                          }
     writeIORef ref newState

     updateDeps cell newValue (deps formula')
     mapM_ invalidateDep rdeps

updateDeps :: (Typeable k, CellValue a) => Cell k a -> Value a -> [Dep] -> IO ()
updateDeps cell value =
  case value of
   Undecided _ -> updateDepsWith insert
   Decided   _ -> updateDepsWith remove

  where dep = Dep cell

        insert rdeps | dep `elem` rdeps = rdeps
                     | otherwise        = dep:rdeps

        remove = delete dep

updateDepsWith :: ([Dep] -> [Dep]) -> [Dep] -> IO ()
updateDepsWith op deps = mapM_ updateOneDep deps
  where updateOneDep (Dep (MkCell _ depCell)) = modifyIORef depCell updateRDeps
          where updateRDeps state@(CellState {..}) = state { rdeps = op rdeps }

ioCell :: a -> IO (Cell 'IOCell a)
ioCell v = MkCell Proxy <$> newIORef state
  where state = CellState {value = Undecided v, rdeps = [], formula = pure v}

getValue :: Cell k a -> IO a
getValue (MkCell _ cell) = readIORef cell >>= return . extractValue . value

setValue :: CellValue a => Cell 'IOCell a -> a -> IO ()
setValue = setValueInternal

modifyValue :: CellValue a => Cell 'IOCell a -> (a -> a) -> IO ()
modifyValue cell f = getValue cell >>= setValue cell . f

setValueInternal :: CellValue a => Cell k a -> a -> IO ()
setValueInternal cell@(MkCell _ ref) v =
  do state@CellState {..} <- readIORef ref

     assertUndecided value "attempting to overwrite decided value"
     when (newValue /= value) $ set state

  where newValue = makeValue v

        set state =
          do writeIORef ref (state {value = newValue})
             updateDeps cell newValue (deps $ formula state)
             mapM_ invalidateDep (rdeps state)

invalidateDep :: Dep -> IO ()
invalidateDep (Dep cell) = invalidate cell

invalidate :: CellValue a => Cell k a -> IO ()
invalidate cell@(MkCell _ ref) =
  do CellState {..} <- readIORef ref

     newValue <- compute formula
     setValueInternal cell newValue

assertUndecided :: Value a -> String -> IO ()
assertUndecided (Decided _) msg = error msg
assertUndecided _ _             = return ()
