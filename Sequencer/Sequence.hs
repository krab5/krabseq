module Sequencer.Sequence (
    Sequence, beatconf,
    newSequence, newEmptySequence, newListSequence,
    resize,
    at, add, remove, clear,
    rawat, rawset,
    window,
) where

import Sequencer.Sequence.Beat
import qualified Data.Vector as V

data Sequence e = Sequence {
    content :: V.Vector e,
    beatconf :: BeatConf,
    noEvent :: e,
    isNoEvent :: e -> Bool,
    defaultGen :: Tick -> e
}

instance Show e => Show (Sequence e) where
  show seq =
      foldl (\acc -> \(id,elt) -> acc ++ (showTickReduce (intToTick bc id)) ++ " " ++ (show elt) ++ "\n") "" (V.indexed $ content seq)
      where bc = beatconf seq

defaultGenerator :: e -> Tick -> e
defaultGenerator noEvent _ = noEvent

defaultIsNoEvent :: Eq e => e -> (e -> Bool)
defaultIsNoEvent e = ((==) e)

newSequence :: BeatConf -> e -> (e -> Bool) -> (Tick -> e) -> Sequence e
newSequence bc noevent isnoevent defaultGen =
    let v = V.generate (tickNumber bc) (defaultGen . (intToTick bc)) in
        Sequence v bc noevent isnoevent defaultGen

newEmptySequence :: BeatConf -> e -> (e -> Bool) -> Sequence e
newEmptySequence bc ne ine =
    let v = V.replicate (tickNumber bc) ne in
        Sequence v bc ne ine (defaultGenerator ne)

newListSequence :: BeatConf -> Sequence [e]
newListSequence bc = newEmptySequence bc [] null

resize :: Sequence e -> BeatConf -> Sequence e
resize s newbc =
    let newct = V.generate (tickNumber newbc) gen in
        s { content = newct, beatconf = newbc }
        where gen n | n >= (V.length ct) = dg $ intToTick newbc n
                    | otherwise          = V.unsafeIndex ct n
              ct = content s
              dg = defaultGen s

at :: Sequence e -> Tick -> Maybe e
at seq t =
    if (isNoEvent seq) evt
        then Nothing
        else Just evt
    where evt = rawat seq t

rawat :: Sequence e -> Tick -> e
rawat seq t
    | (tickToInt t) >= (V.length $ content seq) = defaultGen seq t
    | otherwise = V.unsafeIndex (content seq) (tickToInt t)

rawset :: Sequence e -> Tick -> (e -> e) -> Sequence e
rawset seq tick modifier
    | (tickToInt tick) >= (V.length $ content seq) = seq
    | otherwise =
        seq { content = newseq }
        where index = tickToInt tick
              newelt = modifier $ V.unsafeIndex (content seq) index
              newseq = V.update (content seq) (V.singleton (index, newelt))

add :: Sequence [e] -> Tick -> e -> Sequence [e]
add seq t elt =
    rawset seq t ((:) elt)

clear :: Sequence e -> Tick -> Sequence e
clear seq t =
    rawset seq t (\_ -> noEvent seq)

remove :: (Eq e) => Sequence [e] -> Tick -> e -> Sequence [e]
remove seq t elt =
    rawset seq t (filter (/= elt))

window :: Tick -> Int -> (e -> Bool) -> Sequence [e] -> V.Vector [e]
window start size p seq =
    V.map (filter p) $ V.slice lower tsize ct
    where lower = tickToInt start
          tsize = min ((V.length ct) - lower) size
          ct = content seq





