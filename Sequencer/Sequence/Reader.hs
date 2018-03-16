module Sequencer.Sequence.Reader where

import Data.Foldable
import Sequencer.Sequence
import qualified Sequencer.Sequence.Beat as B
import qualified Sequencer.Sequence.Ticker as T
import Sequencer.Concurrency
import Control.Concurrent
import qualified Control.Concurrent.MVar as MV
import Control.Concurrent.Async.Timer

data ReadSequence e = ReadSequence {
    seq_ :: Sequence e,
    rtick :: T.RTick,
    data_ :: InterruptibleDataChannel e,
    isReading :: Bool
}

current_tick :: ReadSequence e -> B.Tick
current_tick = T.toTick . rtick

current :: ReadSequence e -> Maybe e
current rs =
    if T.isTick (rtick rs)
        then (seq_ rs) `at` (current_tick rs)
        else Nothing

newReadSeq :: Sequence e -> InterruptibleDataChannel e -> Int -> T.TickerConfiguration -> ReadSequence e
newReadSeq seq chan bpm tconf =
    let rt = T.newRTick tconf $ T.TempoBeatConf (beatconf seq) bpm in
    ReadSequence seq rt chan False

seqprogress :: ReadSequence e -> ReadSequence e
seqprogress rs = rs { rtick = T.advanceRTick $ rtick rs  }

seqread :: ReadSequence e -> IO (ReadSequence e)
seqread rs =
    if isReading rs
        then let rs' = seqprogress rs in
                 case current rs' of
                   Nothing -> return rs'
                   Just c  -> (postI (data_ rs) c) >> (return rs')
        else return rs

seqstart :: ReadSequence e -> IO (ReadSequence e)
seqstart rs = return $ rs { isReading = True }

seqpause :: ReadSequence e -> IO (ReadSequence e)
seqpause rs = return $ rs { isReading = False }

seqreset :: ReadSequence e -> IO (ReadSequence e)
seqreset rs = return $ rs { rtick = T.resetRTick $ rtick rs }

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



