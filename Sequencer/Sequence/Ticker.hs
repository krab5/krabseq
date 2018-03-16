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
    current_ :: Int,
    conf :: TickerConfiguration,
    tbc :: TempoBeatConf
} deriving (Eq)

instance Show Ticker where
  show (Ticker d c cf (TempoBeatConf bc bpm)) =
      "[[" ++ show c ++ "/" ++ show d ++ "|" ++ show bc ++ ",BPM=" ++ show bpm ++ "]]"

data TempoBeatConf = TempoBeatConf {
    tbeatconf :: BeatConf,
    tbpm :: BPM
} deriving Eq

instance Show TempoBeatConf where
  show (TempoBeatConf bc bpm) =
      "<<" ++ show bc ++ ",BPM=" ++ show bpm ++ ">>"

data TickerConfiguration = TickerConfiguration {
    atomic_tpm :: Int
} deriving Eq

tcToMicrosec :: TickerConfiguration -> Int
tcToMicrosec (TickerConfiguration tpm) =
    60000000 `div` tpm

tcToNanosec :: TickerConfiguration -> Int
tcToNanosec (TickerConfiguration tpm) =
    60000000000 `div` tpm

minitick :: Ticker -> (Bool,Ticker)
minitick t =
    let c = (current_ t) - 1 in
        if c <= 0
            then (True,  t { current_ = delay t })
            else (False, t { current_ = c })

tickPerMinute :: TempoBeatConf -> Int
tickPerMinute =
    (*) <$> tbpm <*> (tickPerBeat . tbeatconf)

makeConf :: [TempoBeatConf] -> TickerConfiguration
makeConf =
    (TickerConfiguration) . (foldr ((flip lcm) <$> tickPerMinute) 1)

updateConf :: TickerConfiguration -> [TempoBeatConf] -> TickerConfiguration
updateConf (TickerConfiguration old) =
    (TickerConfiguration) . (foldr ((flip lcm) <$> tickPerMinute) old)

makeTicker :: TickerConfiguration -> TempoBeatConf -> Ticker
makeTicker tc tbc =
    let δt = (atomic_tpm tc) `div` (tickPerMinute tbc) in
        Ticker δt 1 tc tbc

-- Note : erase current_
updateTicker :: TickerConfiguration -> TempoBeatConf -> Ticker -> Ticker
updateTicker newconf newtbc old =
    makeTicker newconf newtbc

resetTicker :: Ticker -> Ticker
resetTicker t = t { current_ = 1 }

quantizeOnNextTick :: Ticker -> Tick -> Ticker -> Ticker
quantizeOnNextTick ticker _ sync =
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


data RTickSync = OnTick | OnBeat | OnBar | OnSequence
data RTick = RTick Tick Ticker Bool

isTick :: RTick -> Bool
isTick (RTick _ _ b) = b

toTick :: RTick -> Tick
toTick (RTick t _ _) = t

newRTick :: TickerConfiguration -> TempoBeatConf -> RTick
newRTick tc tbc =
    let ticker = makeTicker tc tbc
        tick = maxTick $ tbeatconf tbc in
        RTick tick ticker False

syncRTick :: RTick -> RTickSync -> RTick -> RTick
syncRTick rtick@(RTick tick ticker _) conf other@(RTick otick oticker _) =
    RTick tick' (qticker conf) False
    where tick' = reset tick
          qtickerf OnTick     = quantizeOnNextTick
          qtickerf OnBeat     = quantizeOnNextBeat
          qtickerf OnBar      = quantizeOnNextBar
          qtickerf OnSequence = quantizeOnNextSequence
          qticker t = (qtickerf t) ticker otick oticker

advanceRTick :: RTick -> RTick
advanceRTick (RTick tick ticker _) =
    let (res,ticker') = minitick ticker' in
        if res
            then RTick (incTick tick) ticker' True
            else RTick tick ticker' False

-- Will trigger a tick on the next-mini tick
resetRTick :: RTick -> RTick
resetRTick (RTick tick ticker _) = 
    RTick (maxTick $ beatConf tick) (resetTicker ticker) False



