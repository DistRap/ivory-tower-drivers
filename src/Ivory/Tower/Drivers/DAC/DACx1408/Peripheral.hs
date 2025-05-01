{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.Drivers.DAC.DACx1408.Peripheral where

import Ivory.Language
import Ivory.HW

import Ivory.Tower.Drivers.DAC.DACx1408.Regs

data DACx1408 = DACx1408
  { dacNop          :: BitDataReg (Bits 16)    -- ^ No-op
  , dacDeviceID     :: BitDataReg DeviceID     -- ^ Device ID
  , dacStatus       :: BitDataReg Status       -- ^ Status
  , dacSPI          :: BitDataReg SPI          -- ^ SPI configuration
  , dacGeneral      :: BitDataReg General      -- ^ General configuration
  , dacBroadcastCfg :: BitDataReg BroadcastCfg -- ^ Broadcast configuration
  , dacSync         :: BitDataReg Sync         -- ^ Synchronization configuration
  , dacToggle0      :: BitDataReg Toggle0      -- ^ DAC[7:4] Toggle configuration
  , dacToggle1      :: BitDataReg Toggle1      -- ^ DAC[3:0] Toggle configuration
  , dacPowerDown    :: BitDataReg PowerDown    -- ^ Power down
  , dacRange0       :: BitDataReg Range        -- ^ DAC[7:4] Range configuration
  , dacRange1       :: BitDataReg Range        -- ^ DAC[3:0] Range configuration
  , dacTrigger      :: BitDataReg Trigger      -- ^ Trigger
  , dacBroadcast    :: BitDataReg (Bits 16)    -- ^ Broadcast data
  , dac0            :: BitDataReg (Bits 16)    -- ^ DAC0 data
  , dac1            :: BitDataReg (Bits 16)    -- ^ DAC1 data
  , dac2            :: BitDataReg (Bits 16)    -- ^ DAC2 data
  , dac3            :: BitDataReg (Bits 16)    -- ^ DAC3 data
  , dac4            :: BitDataReg (Bits 16)    -- ^ DAC4 data
  , dac5            :: BitDataReg (Bits 16)    -- ^ DAC5 data
  , dac6            :: BitDataReg (Bits 16)    -- ^ DAC6 data
  , dac7            :: BitDataReg (Bits 16)    -- ^ DAC7 data
  , offset0         :: BitDataReg (Bits 16)    -- ^ DAC[6-7;4-5] differential offset
  , offset1         :: BitDataReg (Bits 16)    -- ^ DAC[2-3;0-1] differential offset
  }

dacx1408 :: DACx1408
dacx1408 = DACx1408
  { dacNop          = reg 0x00
  , dacDeviceID     = reg 0x01
  , dacStatus       = reg 0x02
  , dacSPI          = reg 0x03
  , dacGeneral      = reg 0x04
  , dacBroadcastCfg = reg 0x05
  , dacSync         = reg 0x06
  , dacToggle0      = reg 0x07
  , dacToggle1      = reg 0x08
  , dacPowerDown    = reg 0x09
  , dacRange0       = reg 0x0b
  , dacRange1       = reg 0x0c
  , dacTrigger      = reg 0x0e
  , dacBroadcast    = reg 0x0f
  , dac0            = reg 0x14
  , dac1            = reg 0x15
  , dac2            = reg 0x16
  , dac3            = reg 0x17
  , dac4            = reg 0x18
  , dac5            = reg 0x19
  , dac6            = reg 0x1A
  , dac7            = reg 0x1B
  , offset0         = reg 0x21
  , offset1         = reg 0x22
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> BitDataReg d
  reg offset = mkBitDataReg offset
