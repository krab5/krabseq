{-# LANGUAGE ScopedTypeVariables #-}
module Sequencer.UI where

import Control.Concurrent
import Control.Exception
import Midi
import Midi.Message
import Sequencer.Concurrency

data ControlUI s = ControlUI {
    inputStream :: Stream,
    outputStream :: Stream,
    inputBufferSize :: Int,
    outputBufferSize :: Int,
    state :: MVar s,
    stoppingChannel :: StoppingChannel,
    eventChannel :: InterruptibleDataChannel Event,
    outputChannel :: InterruptibleDataChannel [Event]
}

newControlUI :: Device -> Int -> Device -> Int -> s -> IO (Maybe (ControlUI s))
newControlUI inputDevice inputBufferSize outputDevice outputBufferSize initialState =
    catch (do
        input <- openInput inputDevice inputBufferSize
        output <- openOutput outputDevice outputBufferSize 0
        stchan <- newStoppingChannel
        evchan <- newInterruptibleDataChannel
        ochan <- newInterruptibleDataChannel
        stmvar <- newMVar initialState
        return $ Just $ ControlUI {
            inputStream = input,
            outputStream = output,
            inputBufferSize = inputBufferSize,
            outputBufferSize = outputBufferSize,
            state = stmvar,
            stoppingChannel = stchan,
            eventChannel = evchan,
            outputChannel = ochan
        }
    ) handle
    where handle (e::Midi.MIDIException) = do
            putStrLn $ "Cannot open controler: " ++ show e
            return Nothing

startControler :: ControlUI s -> (Event -> s -> IO s) -> IO ()
startControler ctrler onInput = do
    tid1 <- forkIO $ readerLoop (inputStream ctrler) (stoppingChannel ctrler) (eventChannel ctrler) (inputBufferSize ctrler)
    tid2 <- forkIO $ dispatcherLoop (stoppingChannel ctrler) (eventChannel ctrler) onInput (state ctrler)
    tid3 <- forkIO $ writerLoop (outputStream ctrler) (stoppingChannel ctrler) (outputChannel ctrler) (outputBufferSize ctrler)
    return ()

stopControler :: ControlUI s -> IO ()
stopControler ctrler = do
    sendEnd $ stoppingChannel ctrler
    interrupt $ eventChannel ctrler
    return ()

readerLoop :: Stream -> StoppingChannel -> InterruptibleDataChannel Event -> Int -> IO ()
readerLoop st endChannel outputChannel buffSize =
    controlledLoop endChannel $ do
        has <- pollInput st
        if has
            then do
                evts <- readInput buffSize st
                postAllI outputChannel evts
            else return ()

cut :: Int -> [a] -> [[a]]
cut n [] = []
cut n l  = (take n l):(cut n (drop n l))

writerLoop :: Stream -> StoppingChannel -> InterruptibleDataChannel [Event] -> Int -> IO ()
writerLoop st endChannel inputChannel buffSize =
    controlledLoop endChannel $ do
        res <- waitForI inputChannel
        case res of
          Just evts -> foreach (cut buffSize evts) $ \block -> writeOutput st block
          Nothing -> return ()

dispatcherLoop :: StoppingChannel -> InterruptibleDataChannel Event -> (Event -> s -> IO s) -> MVar s -> IO ()
dispatcherLoop endChannel inputChannel computation mvstate = do
    controlledLoop endChannel $ do
        res <- waitForI inputChannel
        case res of
          Just evt -> modifyMVar_ mvstate (computation evt)
          Nothing -> return ()


