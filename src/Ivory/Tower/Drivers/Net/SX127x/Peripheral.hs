{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.Drivers.Net.SX127x.Peripheral where

import Ivory.Language
import Ivory.HW

import Ivory.Tower.Drivers.Net.SX127x.Regs

data SX127x = SX127x
  { sxFIFO              :: BitDataReg (Bits 8)
  , sxMode              :: BitDataReg OP_MODE
  , sxFrfMsb            :: BitDataReg (Bits 8)
  , sxFrfMid            :: BitDataReg (Bits 8)
  , sxFrfLsb            :: BitDataReg (Bits 8)
  , sxPAConfig          :: BitDataReg PA_CONFIG
  , sxPARamp            :: BitDataReg PA_RAMP
  , sxOCP               :: BitDataReg OCP
  , sxLNAGain           :: BitDataReg LNA_GAIN
  , sxFIFOAddr          :: BitDataReg (Bits 8)
  , sxFIFOTxBaseAddr    :: BitDataReg (Bits 8)
  , sxFIFORxBaseAddr    :: BitDataReg (Bits 8)
  , sxFIFORxCurrentAddr :: BitDataReg (Bits 8)
  , sxIRQFlagsMask      :: BitDataReg IRQ_FLAGS_MASK
  , sxIRQFlags          :: BitDataReg IRQ_FLAGS
  , sxRxLength          :: BitDataReg (Bits 8)
  , sxRxHeaderCntMsb    :: BitDataReg (Bits 8)
  , sxRxHeaderCntLsb    :: BitDataReg (Bits 8)
  , sxRxPacketCntMsb    :: BitDataReg (Bits 8)
  , sxRxPacketCntLsb    :: BitDataReg (Bits 8)
  , sxModemStatus       :: BitDataReg MODEM_STAT
  , sxPacketSNR         :: BitDataReg (Bits 8)
  , sxPacketRSSI        :: BitDataReg (Bits 8)
  , sxRSSI              :: BitDataReg (Bits 8)
  , sxHopChannel        :: BitDataReg HOP_CHANNEL
  , sxModemConfig1      :: BitDataReg MODEM_CONFIG1
  , sxModemConfig2      :: BitDataReg MODEM_CONFIG2
  , sxSymbolTimeoutLsb  :: BitDataReg (Bits 8)
  , sxPreambleMsb       :: BitDataReg (Bits 8)
  , sxPreambleLsb       :: BitDataReg (Bits 8)
  , sxPayloadLength     :: BitDataReg (Bits 8)
  , sxMaxPayloadLength  :: BitDataReg (Bits 8)
  , sxHopPeriod         :: BitDataReg (Bits 8)
  , sxFIFORxByteAddr    :: BitDataReg (Bits 8)
  , sxModemConfig3      :: BitDataReg MODEM_CONFIG3
  , sxFeiMsb            :: BitDataReg (Bits 8)
  , sxFeiMid            :: BitDataReg (Bits 8)
  , sxFeiLsb            :: BitDataReg (Bits 8)
  , sxRSSIWideband      :: BitDataReg (Bits 8)
  , sxDetectOptimize    :: BitDataReg (Bits 8)
  , sxInvertIQ          :: BitDataReg INVERT_IQ
  , sxDetectThresh      :: BitDataReg (Bits 8)
  , sxSyncWord          :: BitDataReg (Bits 8)

  , sxDIOMapping1       :: BitDataReg DIO_MAPPING1
  , sxDIOMapping2       :: BitDataReg DIO_MAPPING2
  , sxVersion           :: BitDataReg (Bits 8)

  , sxTXCO              :: BitDataReg (Bits 8)
  , sxPADAC             :: BitDataReg (Bits 8)
  , sxFormerTemp        :: BitDataReg (Bits 8)
  , sxAGCRef            :: BitDataReg (Bits 8)
  , sxAGCThresh1        :: BitDataReg (Bits 8)
  , sxAGCThresh2        :: BitDataReg (Bits 8)
  , sxAGCThresh3        :: BitDataReg (Bits 8)
  }



sx127x :: SX127x
sx127x = SX127x
  { sxFIFO              = reg 0x00
  , sxMode              = reg 0x01
  , sxFrfMsb            = reg 0x06
  , sxFrfMid            = reg 0x07
  , sxFrfLsb            = reg 0x08
  , sxPAConfig          = reg 0x09
  , sxPARamp            = reg 0x0a
  , sxOCP               = reg 0x0b
  , sxLNAGain           = reg 0x0c
  , sxFIFOAddr          = reg 0x0d
  , sxFIFOTxBaseAddr    = reg 0x0e
  , sxFIFORxBaseAddr    = reg 0x0f
  , sxFIFORxCurrentAddr = reg 0x10
  , sxIRQFlagsMask      = reg 0x11
  , sxIRQFlags          = reg 0x12
  , sxRxLength          = reg 0x13
  , sxRxHeaderCntMsb    = reg 0x14
  , sxRxHeaderCntLsb    = reg 0x15
  , sxRxPacketCntMsb    = reg 0x16
  , sxRxPacketCntLsb    = reg 0x17
  , sxModemStatus       = reg 0x18
  , sxPacketSNR         = reg 0x19
  , sxPacketRSSI        = reg 0x1a
  , sxRSSI              = reg 0x1b
  , sxHopChannel        = reg 0x1c
  , sxModemConfig1      = reg 0x1d
  , sxModemConfig2      = reg 0x1e
  , sxSymbolTimeoutLsb  = reg 0x1f
  , sxPreambleMsb       = reg 0x20
  , sxPreambleLsb       = reg 0x21
  , sxPayloadLength     = reg 0x22
  , sxMaxPayloadLength  = reg 0x23
  , sxHopPeriod         = reg 0x24
  , sxFIFORxByteAddr    = reg 0x25
  , sxModemConfig3      = reg 0x26
  , sxFeiMsb            = reg 0x28
  , sxFeiMid            = reg 0x29
  , sxFeiLsb            = reg 0x2a
  , sxRSSIWideband      = reg 0x2c
  , sxDetectOptimize    = reg 0x31
  , sxInvertIQ          = reg 0x33
  , sxDetectThresh      = reg 0x37
  , sxSyncWord          = reg 0x39

  , sxDIOMapping1       = reg 0x40
  , sxDIOMapping2       = reg 0x41
  , sxVersion           = reg 0x42

  , sxTXCO              = reg 0x4b
  , sxPADAC             = reg 0x4d
  , sxFormerTemp        = reg 0x5b
  , sxAGCRef            = reg 0x61
  , sxAGCThresh1        = reg 0x62
  , sxAGCThresh2        = reg 0x63
  , sxAGCThresh3        = reg 0x64
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> BitDataReg d
  reg offset = mkBitDataReg offset
