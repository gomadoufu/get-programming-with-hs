{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Cipher where

import Rot13 (rotDecoder, rotEncoder)
import Xor (applyOTP)

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot = rotEncoder
  decode Rot = rotDecoder

data OneTimePad = OneTimePad String

instance Cipher OneTimePad where
  encode (OneTimePad pad) = applyOTP pad
  decode (OneTimePad pad) = applyOTP pad

myOTP :: OneTimePad
myOTP = OneTimePad (cycle [minBound .. maxBound])
