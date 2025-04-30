{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Ivory.Tower.Drivers.Net.LoRa where

import Ivory.Language
import Ivory.Tower

newtype Bandwidth = Bandwidth Uint32
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

bw125, bw250, bw500 :: Bandwidth
[bw125, bw250, bw500]
  = map (Bandwidth . fromInteger) [125_000, 250_000, 500_000]

newtype CodingRate = CodingRate Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

-- 4/(4 + CodingRate) - e.g. cr2 -> 4/6
cr1, cr2, cr3, cr4 :: CodingRate
[cr1, cr2, cr3, cr4]
  = map (CodingRate . fromInteger) [1..4]

newtype SpreadingFactor = SpreadingFactor Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

sf6, sf7, sf8, sf9, sf10, sf11, sf12 :: SpreadingFactor
[sf6, sf7, sf8, sf9, sf10, sf11, sf12]
  = map (SpreadingFactor . fromInteger) [6..12]

-- | Private network sync word
privateNetSyncWord :: Int
privateNetSyncWord = 0x12

-- | The Things Network sync word
ttnSyncWord :: Int
ttnSyncWord = 0x34

-- | Ts - time to transmit one symbol
-- Ts = 2^sf / bw
symbolTime :: SpreadingFactor -> Bandwidth -> ITime
symbolTime (SpreadingFactor sf) (Bandwidth bw) =
  fromIMicroseconds
    $ (castDefault :: IFloat -> Sint32)
    $ 1_000_000 * (safeCast $ 1 `iShiftL` sf) / (safeCast bw)
