module Sequencer.Sequence.Ticker where

{-
   Global principle : try to synchronize everyone on a µs precise universal
   "ticker".
   A ticker is a timer layout that ticks every given delay.
   Each beat configuration + beat per minute yields a particular
   ticker.

   The delay is calculated from a set of beat configuration and beat per minute
   as follow:
     1) for each (bc, bpm) couple, we determinate tpm (number of ticks per minute)
     This is done by multiplicating the bpm with the number of ticker per beat of the
     bc
     2) we determine the least common multiplier of the whole tpm set
     The result of this operation is the total number of "mini-tick" inside a minute
     (called atomic_tpm below)
     3) for each (bc, bpm) couple, we create a ticker, which delay is equal to
     this "atomic tpm" divided by the tpm of the ticker.
     As the atomic tpm is the LCM of every tpms, it is guaranteed that this division
     yields an integer number with no remainder
  
   Quantization:
     We can easily quantize on a tick by synchronizing on another ticker.
     In this case, we simply set the delay of the to-be-quantized delay to the
   current value of the syncing ticker.
     For beat, bar and sequence synchronization, we need to calculate the remaining
   number of mini-ticks to be ellapsed before reaching the next unit.
-}

import Sequencer.Sequence.Beat

type BPM = Int

data Ticker = Ticker {
    delay :: Int,
    current_ :: Int
}

data TempoBeatConf = TempoBeatConf {
    tbeatconf :: BeatConf,
    tbpm :: BPM
}

data TickConfiguration = TickConfiguration {
    atomic_tpm :: Int
}

minitick :: Ticker -> (Bool,Ticker)
minitick t =
    let c = (current_ t) - 1 in
        if c <= 0
            then (True,  t { current_ = delay t })
            else (False, t { current_ = c })

tickPerMinute :: TempoBeatConf -> Int
tickPerMinute =
    (*) <$> tbpm <*> (tickPerBeat . tbeatconf)

makeConf :: [TempoBeatConf] -> TickConfiguration
makeConf =
    (TickConfiguration) . (foldr ((flip lcm) <$> tickPerMinute) 1)

makeTicker :: TickConfiguration -> TempoBeatConf -> Ticker
makeTicker tc tbc =
    let δt = (atomic_tpm tc) `div` (tickPerMinute tbc) in
        Ticker δt 1

quantizeOnNextTick :: Ticker -> Ticker -> Ticker
quantizeOnNextTick ticker sync =
    ticker { delay = current_ sync }

{-
   Remaining mini-tick before next beat =
        (remaining mini-ticks before next tick [Rmt])
    +   (remaining ticks before next beat [Rt]) * (number of mini-ticks per ticks [mtPt])

    -> The number of mini-ticks per ticks is exactly the delay of the ticker
    -> The remaining ticks before next beat is a function of the current tick (easier
    to obtain) : remaining_ticks = tickPerBeat [tPb] - currenttick [ct] - 1
    (the -1 is here because we synchronize on the 0th tick)
    -> The remaining mini-ticks before next tick is given by the "current_" value of
    the ticker
-}
quantizeOnNextBeat_ :: Ticker -> BeatConf -> Int -> Ticker -> Ticker
quantizeOnNextBeat_ ticker syncbc currenttick sync =
    let remaining_miniticks = (current_ sync) + remaining_ticks * (delay sync) in
        ticker { current_ = remaining_miniticks }
    where remaining_ticks = (tickPerBeat syncbc) - currenttick - 1

quantizeOnNextBeat :: Ticker -> Tick -> Ticker -> Ticker
quantizeOnNextBeat ticker synctick syncticker =
    quantizeOnNextBeat_ ticker (beatConf synctick) (tick synctick) syncticker

{-
   Remaining mini-tick before next beat =
        (remaining mini-ticks before next tick [Rmt])
    +   (remaining ticks before next beat [Rt]) * (number of mini-ticks per ticks [mtPt])
    +   (remaining beat before next bar [Rbt]) * (number of mini-ticks per bar [mtPb])

    -> The value with the same name are determined the same way as in quantizeOnNextBeat
    -> The remaining beat before next bar is given by the currentbar, with this formula
       remaining_beats = beatPerBar - currentbeat - 1
    -> The number of mini-ticks per bar is equal to the number of ticks per beat (tPb) multiplied
    by the number of mini-ticks per tick (so delay)

    In other word, the formula above can be rewritten as follow :
       Rmt + Rt*mtPt + Rbt*mtPb
     = Rmt + Rt*mtPt + Rbt*tPb*mtPt
     = Rmt + (Rt + Rbt*tPb)*mtPt
-}
quantizeOnNextBar_ :: Ticker -> BeatConf -> Int -> Int -> Ticker -> Ticker
quantizeOnNextBar_ ticker syncbc currenttick currentbeat sync =
    let remaining_miniticks = (current_ sync) + remaining_ticks * (delay sync) in
        ticker { current_ = remaining_miniticks }
    where remaining_ticks = (tickPerBeat syncbc) - currenttick - 1 + (remaining_beats * (tickPerBeat syncbc))
          remaining_beats = (beatPerBar syncbc) - currentbeat - 1

quantizeOnNextBar :: Ticker -> Tick -> Ticker -> Ticker
quantizeOnNextBar ticker synctick syncticker =
    quantizeOnNextBar_ ticker (beatConf synctick) (tick synctick) (beat synctick) syncticker

quantizeOnNextSequence_ :: Ticker -> BeatConf -> Int -> Int -> Int -> Ticker -> Ticker
quantizeOnNextSequence_ ticker syncbc currenttick currentbeat currentbar sync =
    let remaining_miniticks = (current_ sync) + remaining_ticks * (delay sync) in
        ticker { current_ = remaining_miniticks }
    where remaining_ticks = (tickPerBeat syncbc) - currenttick - 1 + (remaining_beats * (tickPerBeat syncbc))
          remaining_beats = (beatPerBar syncbc) - currentbeat - 1 + (remaining_bars * (beatPerBar syncbc))
          remaining_bars  = (barPerSequence syncbc) - currentbar - 1

quantizeOnNextSequence :: Ticker -> Tick -> Ticker -> Ticker
quantizeOnNextSequence ticker synctick syncticker =
    quantizeOnNextSequence_ ticker (beatConf synctick) (tick synctick) (beat synctick) (bar synctick) syncticker



