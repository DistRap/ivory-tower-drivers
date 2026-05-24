{-# LANGUAGE BinaryLiterals #-}

module Ivory.Tower.Drivers.Magnetometer.QMC5883L.Regs where

import Data.Bits
import Data.Word

data Reg
  = OutXL
  | OutXH
  | OutYL
  | OutYH
  | OutZL
  | OutZH
  | Status
  | TOutL
  | TOutH
  | Ctrl1
  | Ctrl2
  | SetResetPeriod
  | Ident
  deriving (Eq, Show)

regAddr :: Reg -> Word8
regAddr OutXL          = 0x0
regAddr OutXH          = 0x1
regAddr OutYL          = 0x2
regAddr OutYH          = 0x3
regAddr OutZL          = 0x4
regAddr OutZH          = 0x5
regAddr Status         = 0x6
regAddr TOutL          = 0x7
regAddr TOutH          = 0x8
regAddr Ctrl1          = 0x9
regAddr Ctrl2          = 0xA
regAddr SetResetPeriod = 0xB
regAddr Ident          = 0xD

-- MODE
data ModeControl
  = Standby
  | Continuous
  deriving (Eq, Show)

-- ODR
data OutputRate
  = Rate10Hz
  | Rate50Hz
  | Rate100Hz
  | Rate200Hz
  deriving (Eq, Show)

-- RNG
data FullScale
  = Scale2Gauss
  | Scale8Gauss
  deriving (Eq, Show)

-- OSR
data OversampleRatio
  = Oversample512
  | Oversample256
  | Oversample128
  | Oversample64
  deriving (Eq, Show)

ctrl1Val
  :: ModeControl
  -> OutputRate
  -> FullScale
  -> OversampleRatio
  -> Word8
ctrl1Val mode' odr' rng' osr' =
      (osr osr'   `shiftL` 6)
  .|. (rng rng'   `shiftL` 4)
  .|. (odr odr'   `shiftL` 2)
  .|. (mode mode' `shiftL` 0)
  where
  osr Oversample512 = 0b00
  osr Oversample256 = 0b01
  osr Oversample128 = 0b10
  osr Oversample64  = 0b11

  rng Scale2Gauss = 0b00
  rng Scale8Gauss = 0b01

  odr Rate10Hz  = 0b00
  odr Rate50Hz  = 0b01
  odr Rate100Hz = 0b10
  odr Rate200Hz = 0b11

  mode Standby    = 0b00
  mode Continuous = 0b01
