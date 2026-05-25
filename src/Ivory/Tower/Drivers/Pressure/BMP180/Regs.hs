{-# LANGUAGE BinaryLiterals #-}

module Ivory.Tower.Drivers.Pressure.BMP180.Regs where

import Data.Bits
import Data.Word

data Reg
  = Calib -- 0xAA .. 0xBF, 22 registers
  | Ident
  | Reset
  | Ctrl
  | Out -- 0xF6 .. 0xF8, 3 registers (MSB, LSB, XLSB)
  deriving (Eq, Show)

regAddr :: Reg -> Word8
regAddr Calib = 0xAA
regAddr Ident = 0xD0
regAddr Reset = 0xE0
regAddr Ctrl  = 0xF4
regAddr Out   = 0xF6

data Oversample
  = Oversample1
  | Oversample2
  | Oversample4
  | Oversample8
  deriving (Eq, Show)

data MeasurementMode
  = MeasurementMode_Temperature
  | MeasurementMode_Pressure
  deriving (Eq, Show)

ctrlVal :: MeasurementMode -> Oversample -> Word8
ctrlVal MeasurementMode_Temperature _ = 0x2E
ctrlVal MeasurementMode_Pressure o    = 0x34 .|. (osVal o `shiftL` 6)

osVal :: Oversample -> Word8
osVal Oversample1 = 0b00
osVal Oversample2 = 0b01
osVal Oversample4 = 0b10
osVal Oversample8 = 0b11
