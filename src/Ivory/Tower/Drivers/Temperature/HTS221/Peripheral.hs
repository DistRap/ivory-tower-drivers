{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.Drivers.Temperature.HTS221.Peripheral where

import Ivory.Language
import Ivory.HW

import Ivory.Tower.Drivers.Temperature.HTS221.Regs


data HTS221 = HTS221
  { htsWhoAmI         :: BitDataReg (Bits 8)
  , htsAvConf         :: BitDataReg AVCONF
  , htsCtrl1          :: BitDataReg CTRL1
  , htsCtrl2          :: BitDataReg CTRL2
  , htsCtrl3          :: BitDataReg CTRL3
  , htsStatus         :: BitDataReg STATUS
  , htsHumiLow        :: BitDataReg (Bits 16)
  , htsTempLow        :: BitDataReg (Bits 16)
  , htsCalH0_RH_x2    :: BitDataReg (Bits 8)
  , htsCalH1_RH_x2    :: BitDataReg (Bits 8)
  , htsCalT0_DegC_x8  :: BitDataReg (Bits 8)
  , htsCalT1_DegC_x8  :: BitDataReg (Bits 8)
  , htsCalT1T0Msb     :: BitDataReg (Bits 8) -- first 4 bits are reserved
  , htsCalH0T0OutLow  :: BitDataReg (Bits 16)
  , htsCalH1T0OutLow  :: BitDataReg (Bits 16)
  , htsCalT0OutLow    :: BitDataReg (Bits 16)
  , htsCalT1OutLow    :: BitDataReg (Bits 16)
  }

hts221 :: HTS221
hts221 = HTS221
  { htsWhoAmI         = reg 0xF
  , htsAvConf         = reg 0x10
  , htsCtrl1          = reg 0x20
  , htsCtrl2          = reg 0x21
  , htsCtrl3          = reg 0x23
  , htsStatus         = reg 0x27
  , htsHumiLow        = reg 0x28
  , htsTempLow        = reg 0x2A
  , htsCalH0_RH_x2    = reg 0x30
  , htsCalH1_RH_x2    = reg 0x31
  , htsCalT0_DegC_x8  = reg 0x32
  , htsCalT1_DegC_x8  = reg 0x33
  , htsCalT1T0Msb     = reg 0x35
  , htsCalH0T0OutLow  = reg 0x36
  , htsCalH1T0OutLow  = reg 0x3A
  , htsCalT0OutLow    = reg 0x3C
  , htsCalT1OutLow    = reg 0x3E
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> BitDataReg d
  reg offset = mkBitDataReg offset

