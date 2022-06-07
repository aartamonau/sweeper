module Utils.Chan
  ( Chan,
    Delay,
    new,
    newDelay,
    close,
    put,
    take,
  )
where

import Control.Concurrent.STM
  ( STM,
    TMVar,
    TVar,
    atomically,
    newEmptyTMVarIO,
    newTVarIO,
    orElse,
    putTMVar,
    readTVar,
    retry,
    takeTMVar,
    writeTVar,
  )
import Control.Concurrent.STM.Delay (Delay, newDelay, waitDelay)
import Control.Monad.Extra (ifM, whenM)
import Prelude hiding (take)

data Chan a = Chan
  { closed :: TVar Bool,
    box :: TMVar a
  }

new :: IO (Chan a)
new = Chan <$> newTVarIO False <*> newEmptyTMVarIO

close :: Chan a -> IO ()
close chan@(Chan {closed}) =
  atomically $ do
    errorIfClosed "Chan.close" chan
    writeTVar closed True

put :: Chan a -> a -> Delay -> IO Bool
put chan@(Chan {box}) value delay =
  atomically $ do
    errorIfClosed "Chan.put" chan
    (waitDelay delay >> return False)
      `orElse` (putTMVar box value >> return True)

take :: Chan a -> IO (Maybe a)
take chan@(Chan {box}) =
  atomically $ tryTake `orElse` checkClosed
  where
    tryTake = Just <$> takeTMVar box
    checkClosed = ifM (isClosed chan) (return Nothing) retry

errorIfClosed :: String -> Chan a -> STM ()
errorIfClosed op chan = whenM (isClosed chan) (error msg)
  where
    msg = op ++ ": channel closed"

isClosed :: Chan a -> STM Bool
isClosed = readTVar . closed
