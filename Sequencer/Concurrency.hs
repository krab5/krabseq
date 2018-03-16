module Sequencer.Concurrency where

import Data.Maybe
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar as MV
import qualified Control.Concurrent.MSem as Sem
import Control.Monad.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.AlarmClock
import System.Clock

foreach :: Foldable t => t a -> (a -> IO ()) -> IO ()
foreach elts computation =
    foldl (\acc -> \elt -> acc >> (computation elt)) (return ()) elts

type StoppingChannel = MV.MVar ()

newStoppingChannel :: IO StoppingChannel
newStoppingChannel = newEmptyMVar

sendEnd :: StoppingChannel -> IO ()
sendEnd = flip MV.putMVar $ ()

hasStop :: StoppingChannel -> IO Bool
hasStop chan = (MV.tryReadMVar chan) >>= (return . isJust)

controlledLoop :: StoppingChannel -> IO a -> IO a
controlledLoop channel computation = go
    where
      go = do
        stop <- hasStop channel
        last <- computation
        if stop
            then return last
            else go

type DataChannel a = TQueue a

newDataChannel :: IO (DataChannel a)
newDataChannel = atomically $ newTQueue

post :: DataChannel a -> a -> IO ()
post que = atomically . (writeTQueue que)

postAll :: DataChannel a -> [a] -> IO ()
postAll que elts =
    foreach elts (\elt -> atomically $ writeTQueue que elt)

waitFor :: DataChannel a -> IO a
waitFor = atomically . readTQueue


data InterruptibleDataChannel a = InterruptibleDataChannel (Sem.MSem Int) (DataChannel a)

newInterruptibleDataChannel :: IO (InterruptibleDataChannel a)
newInterruptibleDataChannel = do
    que <- newDataChannel
    sem <- Sem.new 0
    return (InterruptibleDataChannel sem que)

postI :: InterruptibleDataChannel a -> a -> IO ()
postI (InterruptibleDataChannel sem que) elt = do
    atomically $ writeTQueue que elt
    Sem.signal sem

postAllI :: InterruptibleDataChannel a -> [a] -> IO ()
postAllI (InterruptibleDataChannel sem que) elts = do
    foreach elts (\elt -> atomically $ writeTQueue que elt)
    foreach elts (\_ -> Sem.signal sem)

waitForI :: InterruptibleDataChannel a -> IO (Maybe a)
waitForI (InterruptibleDataChannel sem que) =
    Sem.wait sem >> (atomically $ tryReadTQueue que)

interrupt :: InterruptibleDataChannel a -> IO ()
interrupt (InterruptibleDataChannel sem _) =
    Sem.signal sem

interruptAndFlush :: InterruptibleDataChannel a -> IO [a]
interruptAndFlush (InterruptibleDataChannel sem que) =
    Sem.signal sem >> (atomically $ flushTQueue que)

instance TimeScale TimeSpec where
  getAbsoluteTime = getTime Monotonic
  earlierOf = min
  microsecondsDiff t1 t2 = (`div` 1000) $ toNanoSecs $ diffTimeSpec t1 t2

rerun :: TimeSpec -> StoppingChannel -> IO () -> AlarmClock TimeSpec -> IO ()
rerun ts end compute alarm = do
    setAlarm alarm ts
    stopped <- hasStop end
    if stopped
        then destroyAlarmClock alarm
        else compute

startAlarm :: TimeSpec -> StoppingChannel -> IO () -> IO ()
startAlarm ts end compute =
    (newAlarmClock $ rerun ts end compute) >>= setAlarmNow




