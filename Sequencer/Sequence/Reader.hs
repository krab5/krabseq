module Sequencer.Sequence.Reader where

import Data.Foldable
import Sequencer.Sequence
import qualified Sequencer.Sequence.Beat as B
import Sequencer.Concurrency
import Control.Concurrent
import qualified Control.Concurrent.MVar as MV
import Control.Concurrent.Async.Timer

data ReadSequence e = ReadSequence {
    seq_ :: Sequence e,
    current_tick :: B.Tick,
    data_ :: InterruptibleDataChannel e,
    isReading :: Bool
}

current :: ReadSequence e -> e
current rs = (seq_ rs) `at` (current_tick rs)

newReadSeq :: Sequence e -> InterruptibleDataChannel e -> ReadSequence e
newReadSeq seq chan =
    ReadSequence seq (B.mkTick $ beatconf seq) chan False

seqnext :: ReadSequence e -> ReadSequence e
seqnext rs = rs { current_tick = (B.incTick $ current_tick rs) }

seqread :: ReadSequence e -> IO (ReadSequence e)
seqread rs =
    if isReading rs
        then let n = seqnext rs in
                 (postI (data_ rs) ((seq_ rs) `at` (current_tick rs))) >>
                 (return rs)
        else return rs

seqstart :: ReadSequence e -> IO (ReadSequence e)
seqstart rs = return $ rs { isReading = True }

seqpause :: ReadSequence e -> IO (ReadSequence e)
seqpause rs = return $ rs { isReading = False }

seqreset :: ReadSequence e -> IO (ReadSequence e)
seqreset rs = return $ rs { current_tick = B.reset $ current_tick rs }

seqstop :: ReadSequence e -> IO (ReadSequence e)
seqstop rs = (seqpause rs) >>= seqreset


data SequenceReader e = SequenceReader {
    readseq_ :: MVar [MVar (ReadSequence e)],
    bpm :: Int
}

doTick :: SequenceReader e -> IO ()
doTick seqreader =
    modifyMVar_ (readseq_ seqreader) $ \sequences -> do
        forM_ sequences $ (\seq -> modifyMVar_ seq seqread)
        return sequences

bpmToMillisec :: BeatConf -> Int -> Int
bpmToMillisec

startReader :: SequenceReader e -> StoppingChannel -> IO ()
startReader reader end =
    controlledLoop end $ do
        withAsyncTimer timerconf $ \timer -> do
            wait timer
            doTick reader
    where timerconf = bpmToMillisec $ bpm reader



