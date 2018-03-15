module Sequencer.Sequence (
    Sequence,
    newSequence, newEmptySequence, newListSequence,
    resize,
    at, add, remove, clear, vmap,
    window,
) where

import Sequencer.Sequence.Beat
import qualified Data.Vector as V

data Sequence e = Sequence {
    content :: V.Vector e,
    beatconf :: BeatConf,
    noEvent :: e,
    defaultGen :: Tick -> e
}

instance Show e => Show (Sequence e) where
  show seq =
      foldl (\acc -> \(id,elt) -> acc ++ (showTickReduce (intToTick bc id)) ++ " " ++ (show elt) ++ "\n") "" (V.indexed $ content seq)
      where bc = beatconf seq

instance Functor Sequence where
  -- fmap :: (a -> b) -> Sequence a -> Sequence b
  fmap f (Sequence ct bc ne dg) =
      Sequence (V.map f ct) bc (f ne) (f . dg)

instance Applicative Sequence where
  -- pure :: a -> Sequence a
  pure e = Sequence (V.empty) (mkBeatConf 0 0 0) e (\_ -> e)

  -- (<*>) :: Sequence (a -> b) -> Sequence a -> Sequence b
  (Sequence fs bc1 fne fdg) <*> (Sequence a bc2 ne dg) =
      Sequence nct tbc (fne ne) (\t -> fdg t $ dg t)
      where tbc = bcmin bc1 bc2
            nct = V.generate (tickNumber tbc) $ \n -> (V.unsafeIndex fs n) (V.unsafeIndex a n)


defaultGenerator :: e -> Tick -> e
defaultGenerator noEvent _ = noEvent

newSequence :: BeatConf -> e -> (Tick -> e) -> Sequence e
newSequence bc noevent defaultGen =
    let v = V.generate (tickNumber bc) (defaultGen . (intToTick bc)) in
        Sequence v bc noevent defaultGen

newEmptySequence :: BeatConf -> e -> Sequence e
newEmptySequence bc ne =
    let v = V.replicate (tickNumber bc) ne in
        Sequence v bc ne (defaultGenerator ne)

newListSequence :: BeatConf -> Sequence [e]
newListSequence bc = newEmptySequence bc []

resize :: Sequence e -> BeatConf -> Sequence e
resize s@(Sequence ct bc ne dg) newbc =
    let newct = V.generate (tickNumber newbc) gen in
        Sequence newct newbc ne dg
        where gen n | n >= (V.length ct) = dg $ intToTick newbc n
                    | otherwise          = V.unsafeIndex ct n

at :: Sequence e -> Tick -> e
at seq t 
    | (tickToInt t) >= (V.length $ content seq) = defaultGen seq t
    | otherwise = V.unsafeIndex (content seq) (tickToInt t)

rawset :: Sequence e -> Tick -> (e -> e) -> Sequence e
rawset seq tick modifier
    | (tickToInt tick) >= (V.length $ content seq) = seq
    | otherwise =
        Sequence newseq (beatconf seq) (noEvent seq) (defaultGen seq)
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

vmap :: ([a] -> [b]) -> Sequence [a] -> Sequence [b]
vmap f s =
    Sequence ((V.map f) $ content s) (beatconf s) (f $ noEvent s) (f . (defaultGen s))

window :: Tick -> Int -> (e -> Bool) -> Sequence [e] -> V.Vector [e]
window start size p (Sequence ct bc ne dg) =
    V.map (filter p) $ V.slice lower tsize ct
    where lower = tickToInt start
          tsize = min ((V.length ct) - lower) size




