{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.Drivers.Temperature.SI7006.Peripheral where

import Ivory.Language
import Ivory.HW

import Ivory.Tower.Drivers.Temperature.SI7006.Regs

data I2CReg a = I2CReg {
    regRead  :: [Int]
  , regWrite :: [Int]
  , regBytes :: Int
  , regData  :: BitDataReg a
  }

data SI7006 = SI7006
  { siUser            :: I2CReg USER
  , siHeater          :: I2CReg HEAT
  , siFwRevision      :: I2CReg (Bits 8)
  , siPrevTemp        :: I2CReg (Bits 16)
  , siTemp            :: I2CReg (Bits 16)
  , siHumi            :: I2CReg (Bits 16)
  , siIDLow           :: I2CReg (Bits 32)
  , siIDHigh          :: I2CReg (Bits 32)
  }

data Command =
   MeasureTempHold
 | MeasureTempNoHold
 | MeasureHumiHold
 | MeasureHumiNoHold
 | ReadPreviousTemp
 | Reset
 | WriteUser
 | ReadUser
 | WriteHeater
 | ReadHeater
 | ReadIDLow
 | ReadIDHigh
 | ReadFwRevision
 deriving (Eq)

commandCode :: Command -> [Int]
commandCode MeasureTempHold   = [0xE3]
commandCode MeasureTempNoHold = [0xF3]
commandCode MeasureHumiHold   = [0xE5]
commandCode MeasureHumiNoHold = [0xF5]
commandCode ReadPreviousTemp  = [0xE0]
commandCode Reset             = [0xFE]
commandCode WriteUser         = [0xE6]
commandCode ReadUser          = [0xE7]
commandCode WriteHeater       = [0x51]
commandCode ReadHeater        = [0x11]
commandCode ReadIDLow         = [0xFA, 0x0F]
commandCode ReadIDHigh        = [0xFC, 0xC9]
commandCode ReadFwRevision    = [0x84, 0xB8]

si7006 :: SI7006
si7006 = SI7006
  { siUser       = i2cReg ReadUser WriteUser 1
  , siHeater     = i2cReg ReadHeater WriteHeater 1
  , siFwRevision = i2cRegReadOnly ReadFwRevision 1
  , siPrevTemp   = i2cRegReadOnly ReadPreviousTemp 3
  , siTemp       = i2cRegReadOnly MeasureTempHold  3
  , siHumi       = i2cRegReadOnly MeasureHumiHold  3
  , siIDLow      = i2cRegReadOnly ReadIDLow   8
  , siIDHigh     = i2cRegReadOnly ReadIDHigh  6

  }
  where
  i2cReg :: (IvoryIOReg (BitDataRep d)) => Command -> Command -> Int -> I2CReg d
  i2cReg readCmd writeCmd regNBytes = I2CReg (commandCode readCmd) (commandCode writeCmd) regNBytes (mkBitDataReg 0x0)
  i2cRegReadOnly readCmd = i2cReg readCmd (error "Read only register")
