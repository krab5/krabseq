module Sequencer.UI.Pages where

import Sequencer.Concurrency
import Midi

class MultiPage a where
  process :: Event -> a -> IO a
  transition :: Event -> a -> Maybe a
  isTrigger :: Event -> a -> Bool
  same :: a -> a -> Bool
  transmit :: a -> IO [Event]

doTransition :: MultiPage a => Event -> a -> IO a
doTransition evt mp =
    case transition evt mp of
      Nothing  -> return mp
      Just mp' ->
          if same mp mp'
              then return mp
              else return mp'

dispatch :: MultiPage a => Event -> a -> IO a
dispatch evt mp =
    if isTrigger evt mp
        then doTransition evt mp
        else process evt mp

writeMidi :: MultiPage a => a -> InterruptibleDataChannel [Event] -> IO ()
writeMidi mp chan = (transmit mp) >>= (\evts -> postI chan evts)



