module Sequencer.Sequence.Beat (
    BeatConf, barPerSequence, beatPerBar, tickPerBeat,
    mkBeatConf, tickNumber,
    Tick, bar, beat, tick, beatConf,
    bcmin,
    showTickReduce,
    mkTick, reset, maxTick,
    incTick, incBeat, incBar,
    tickToInt, intToTick
) where

import Data.Ix

data BeatConf = BeatConf {
    barPerSequence :: Int,
    beatPerBar :: Int,
    tickPerBeat :: Int
} deriving Eq

data Tick = Tick {
    bar :: Int,
    beat :: Int,
    tick :: Int,
    beatConf :: BeatConf
 } deriving Eq

instance Show BeatConf where
  show (BeatConf bps bpb tpb) =
      "<barPerSequence=" ++ (show bps) ++
      ", beatPerPar=" ++ (show bpb) ++
      ", tickPerBeat=" ++ (show tpb) ++ ">"

instance Show Tick where
  show (Tick bar beat tick (BeatConf bps bpb tpc)) =
      "<" ++ (show bar) ++ ":" ++ (show bps) ++ ";" ++
             (show beat) ++ ":" ++ (show bpb) ++ ";" ++
             (show tick) ++ ":" ++ (show tpc) ++ ">"

instance Ord Tick where
  compare t1 t2 = compare (tickToInt t1) (tickToInt t2)

instance Ix Tick where
  -- range :: (a,a) -> [a]
  range (l,u) = map (intToTick (beatConf l)) $ range (tickToInt l, tickToInt u)
  index (l,u) e
      | e < l || e > u = -1
      | otherwise = (tickToInt e) - (tickToInt l)
  inRange (l,u) e =
      (l <= e && e <= u)

bcmin :: BeatConf -> BeatConf -> BeatConf
bcmin bc1 bc2 =
    let ibc1 = tickNumber bc1
        ibc2 = tickNumber bc2 in
        if ibc1 <= ibc2
            then bc1
            else bc2

showTickReduce :: Tick -> [Char]
showTickReduce (Tick bar beat tick _) = (show bar) ++ (show beat) ++ (show tick)

mkBeatConf :: Int -> Int -> Int -> BeatConf
mkBeatConf bps bpb tpb = BeatConf bps bpb tpb

tickNumber :: BeatConf -> Int
tickNumber (BeatConf bps bpb tpb) = tpb * bpb * bps

mkTick :: BeatConf -> Tick
mkTick bc = Tick 0 0 0 bc

incTick :: Tick -> Tick
incTick t@(Tick bar beat tick bc@(BeatConf bps bpb tpb)) =
    let tick_plus = (tick + 1) `mod` tpb in
        if tick_plus == 0 then
            incBeat t
        else
            Tick bar beat tick_plus bc

incBeat :: Tick -> Tick
incBeat t@(Tick bar beat tick bc@(BeatConf bps bpb tpb)) =
    let beat_plus = (beat + 1) `mod` bpb in
        if beat_plus == 0 then
            incBar t
        else
            Tick bar beat_plus 0 bc

incBar :: Tick -> Tick
incBar t@(Tick bar beat tick bc@(BeatConf bps bpb tpb)) =
    let bar_plus = (bar + 1) `mod` bps in
        if bar_plus == 0 then
            reset t
        else
            Tick bar_plus 0 0 bc

reset :: Tick -> Tick
reset (Tick _ _ _ bc) = Tick 0 0 0 bc

maxTick :: BeatConf -> Tick
maxTick bc =
    Tick ((barPerSequence bc) - 1) ((beatPerBar bc) - 1) ((tickPerBeat bc) - 1) bc

tickToInt :: Tick -> Int
tickToInt (Tick bar beat tick (BeatConf _ bpb tpb)) =
    (bar*bpb + beat)*tpb + tick

intToTick :: BeatConf -> Int -> Tick
intToTick bc@(BeatConf bps bpb tpb) n =
    let tick = n `mod` tpb
        quo  = n `div` tpb in
        let beat = quo `mod` bpb
            bar  = (quo `div` bpb) `mod` bps in
            Tick bar beat tick bc


