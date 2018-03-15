module Sequencer.UI.LaunchpadMini (
    LedBrightness(..),
    LedConfiguration(..),
    LMMessage(..),
    isPressed, toLM, toMsg,
    ) where

import Data.Bits ((.&.),(.|.),shift)
import Data.Word (Word8)
import Midi.Message
import Sequencer.UI

data LedBrightness = Off | Low | Medium | Full deriving (Show,Eq,Ord,Enum)

data LedConfiguration = LedConfiguration { red :: LedBrightness, green :: LedBrightness, clear :: Bool, copy :: Bool }

toVelocity :: LedConfiguration -> Word8
toVelocity (LedConfiguration r g cl cp) =
    let ri  = fromIntegral $ (fromEnum r) .&. 0x03
        gi  = fromIntegral $ (fromEnum g) .&. 0x03
        cli = fromIntegral $ (fromEnum cl) .&. 0x01
        cpi = fromIntegral $ (fromEnum cp) .&. 0x01 in
        ri .|. (shift cpi 2) .|. (shift cli 3) .|. (shift gi 4)

data LMMessage =
          Grid Word8 Word8 Bool
        | TopRow Word8 Bool
        | RightColumn Word8 Bool

isPressed :: LMMessage -> Bool
isPressed (Grid _ _ b) = b
isPressed (TopRow _ b) = b
isPressed (RightColumn _ b) = b

toLM :: Message -> Maybe LMMessage
toLM (NoteOff 0 key vel) =
    let row = key `div` 0x10
        col = key `mod` 0x10 in
        if row < 0 || row > 7 || col < 0 || col > 8
            then Nothing
            else
                if col == 8
                    then Just $ RightColumn row (vel /= 0)
                    else Just $ Grid row col (vel /= 0)
toLM (ControlChange 0 key val) =
    if key > 0x6F || key < 0x68
        then Nothing
        else Just $ TopRow (key - 0x68) (val /= 0)
toLM _ = Nothing

toMsg :: LMMessage -> LedConfiguration -> Message
toMsg (Grid row col _)    lc = NoteOff 0 (row * 0x10 + col) (toVelocity lc)
toMsg (RightColumn row _) lc = NoteOff 0 (row * 0x10 + 8) (toVelocity lc)
toMsg (TopRow col _)      lc = ControlChange 0 (col + 0x68) (toVelocity lc)



