{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.Net.SX127x.RegTypes where

import Ivory.Language

[ivory|
 bitdata Mode :: Bits 3
   = mode_sleep        as 0b000
   | mode_standby      as 0b001
   | mode_fstx         as 0b010 -- Frequency synthesis TX
   | mode_tx           as 0b011
   | mode_fsrx         as 0b100 -- Frequency synthesis RX
   | mode_rxcontinuous as 0b101
   | mode_rxsingle     as 0b110
   | mode_cad          as 0b111 -- Channel activity detection

 bitdata PaRamp :: Bits 4
   = pa_ramp_3ms4      as 0b0000
   | pa_ramp_2ms       as 0b0001
   | pa_ramp_1ms       as 0b0010
   | pa_ramp_500us     as 0b0011
   | pa_ramp_250us     as 0b0100
   | pa_ramp_125us     as 0b0101
   | pa_ramp_100us     as 0b0110
   | pa_ramp_62us      as 0b0111
   | pa_ramp_50us      as 0b1000
   | pa_ramp_40us      as 0b1001
   | pa_ramp_31us      as 0b1010
   | pa_ramp_25us      as 0b1011
   | pa_ramp_20us      as 0b1100
   | pa_ramp_15us      as 0b1101
   | pa_ramp_12us      as 0b1110
   | pa_ramp_10us      as 0b1111

 bitdata LnaGain :: Bits 3
   = lna_gain_g1       as 0b001 -- maximum gain
   | lna_gain_g2       as 0b010
   | lna_gain_g3       as 0b011
   | lna_gain_g4       as 0b100
   | lna_gain_g5       as 0b101
   | lna_gain_g6       as 0b110 -- minimum gain

 bitdata Bandwidth :: Bits 4
   = bandwidth_7_8_kHz   as 0b0000
   | bandwidth_10_4_kHz  as 0b0001
   | bandwidth_15_6_kHz  as 0b0010
   | bandwidth_20_8_kHz  as 0b0011
   | bandwidth_31_25_kHz as 0b0100
   | bandwidth_41_7_kHz  as 0b0101
   | bandwidth_62_5_kHz  as 0b0110
   | bandwidth_125_kHz   as 0b0111 -- default
   | bandwidth_250_kHz   as 0b1000
   | bandwidth_500_kHz   as 0b1001

  bitdata CodingRate :: Bits 3
   = cr_4over5bits as 0b001
   | cr_4over6bits as 0b010
   | cr_4over7bits as 0b011
   | cr_4over8bits as 0b100

 bitdata SpreadingFactor :: Bits 4
   = spreadingfactor_6   as 6
   | spreadingfactor_7   as 7
   | spreadingfactor_8   as 8
   | spreadingfactor_9   as 9
   | spreadingfactor_10  as 10
   | spreadingfactor_11  as 11
   | spreadingfactor_12  as 12

 bitdata LoRaDIO0 :: Bits 2
   = loraDIO0RxDone     as 0b00
   | loraDIO0TxDone     as 0b01
   | loraDIO0CadDone    as 0b10
   | loraDIO0RxOrTxDone as 0b11 -- undocumented

 bitdata LoRaDIO1 :: Bits 2
   = loraDIO1RxTimeout         as 0b00
   | loraDIO1FhssChangeChannel as 0b01
   | loraDIO1CadDetected       as 0b10

 bitdata LoRaDIO2 :: Bits 2
   = loraDIO2FhssChangeChannel as 0b00
   -- rest are the same as ^

 bitdata LoRaDIO3 :: Bits 2
   = loraDIO3CadDone           as 0b00
   | loraDIO3ValidHeader       as 0b01
   | loraDIO3PayloadCRCError   as 0b11

 bitdata LoRaDIO4 :: Bits 2
   = loraDIO4CadDetected       as 0b00
   | loraDIO4PllLock           as 0b01

 bitdata LoRaDIO5 :: Bits 2
   = loraDIO5ModeReady         as 0b00
   | loraDIO5ClkOut            as 0b01

 bitdata PADACMode :: Bits 3
   = paDacModeDefault          as 0x04
   | paDacModeBoost            as 0x07 -- +20dBM on PA_BOOST
|]
