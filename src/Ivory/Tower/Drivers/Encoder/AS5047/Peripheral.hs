{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.Drivers.Encoder.AS5047.Peripheral where

import Ivory.Language
import Ivory.HW

import Ivory.Tower.Drivers.Encoder.AS5047.Regs

type ASReg = BitDataReg (Bits 14)

data AS5047 = AS5047
  { 
    -- volatile registers
    asNop       :: BitDataReg (Bits 14) -- ^ No-op
  , asErrfl     :: BitDataReg ERRFL     -- ^ Error register
  , asProg      :: BitDataReg PROG      -- ^ Programming register
  , asDiagAGC   :: BitDataReg DIAGAGC   -- ^ Diagnostic info and AGC control value
  , asMag       :: BitDataReg MAG       -- ^ CORDIC magnitude information
  , asAngle     :: BitDataReg CORDICANG -- ^ Angle information without dynamic angle error compensation
  , asAnglecom  :: BitDataReg DAECANG   -- ^ Angle information with dynamic angle error compensation
    -- non-volatile registers
  , asZposM     :: BitDataReg (Bits 8)  -- ^ Zero position MSB
  , asZposL     :: BitDataReg ZPOSL     -- ^ Zero position LSB / MAG diagnostic
  , asSettings1 :: BitDataReg SETTINGS1 -- ^ Settings
  , asSettings2 :: BitDataReg SETTINGS2 -- ^ Settings continued
  }

as5047 :: AS5047
as5047 = AS5047
  {
    -- volatile registers
    asNop       = reg 0x0000
  , asErrfl     = reg 0x0001
  , asProg      = reg 0x0003
  , asDiagAGC   = reg 0x3FFC
  , asMag       = reg 0x3FFD
  , asAngle     = reg 0x3FFE
  , asAnglecom  = reg 0x3FFF
    -- non-volatile registers
  , asZposM     = reg 0x0016
  , asZposL     = reg 0x0017
  , asSettings1 = reg 0x0018
  , asSettings2 = reg 0x0019
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> BitDataReg d
  reg offset = mkBitDataReg offset
