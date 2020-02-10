{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.ADC.ADS1x1x.RegTypes where

import Ivory.Language

[ivory|
 bitdata AdsReg :: Bit
   = adsRegConversion as 0b00
   | adsRegConfig     as 0b01
   | adsRegThreshLow  as 0b10
   | adsRegThreshHigh as 0b11

 bitdata OperationalStatus :: Bit
   = opStatusNoop   as 0b0 -- No effect
   | opStatusSingle as 0b1 -- Perform single conversion

 bitdata MUX :: Bits 3
   = muxDiff_0_1 as 0b000 -- Differential P = AIN0, N = AIN1 (default)
   | muxDiff_0_3 as 0b001 -- Differential P = AIN0, N = AIN3
   | muxDiff_1_3 as 0b010 -- Differential P = AIN1, N = AIN3
   | muxDiff_2_3 as 0b011 -- Differential P = AIN2, N = AIN3
   | muxSingle_0 as 0b100 -- Single-ended AIN0
   | muxSingle_1 as 0b101 -- Single-ended AIN1
   | muxSingle_2 as 0b110 -- Single-ended AIN2
   | muxSingle_3 as 0b111 -- Single-ended AIN3

 bitdata PGA :: Bits 3
   = pga_fsr_6_144 as 0b000
   | pga_fsr_4_096 as 0b001
   | pga_fsr_2_048 as 0b010 -- default
   | pga_fsr_1_024 as 0b011
   | pga_fsr_0_512 as 0b100
   | pga_fsr_0_256 as 0b111

 bitdata Mode :: Bit
   = modeContinuous as 0b0
   | modeSingle     as 0b1

 bitdata DataRate :: Bits 3
   = dr_128  as 0b000
   | dr_250  as 0b001
   | dr_490  as 0b010
   | dr_920  as 0b011
   | dr_1600 as 0b100 --default
   | dr_2400 as 0b101
   | dr_3300 as 0b111

 bitdata ComparatorMode :: Bit
   = cmpModeTraditional as 0b0
   | cmpModeWindow      as 0b1

 bitdata ComparatorPolarity :: Bit
   = cmpPolarityActiveLow  as 0b0
   | cmpPolarityActiveHigh as 0b1

 bitdata ComparatorLatching :: Bit
   = cmpNonLatching as 0b0
   | cmpLatching    as 0b1

 bitdata ComparatorQueue :: Bits 2
   = cmpQueueAssert1 as 0b00
   | cmpQueueAssert2 as 0b01
   | cmpQueueAssert3 as 0b10
   | cmpQueueDisable as 0b11 -- Disable comparator, put alert/rdy pin to HiZ
|]
