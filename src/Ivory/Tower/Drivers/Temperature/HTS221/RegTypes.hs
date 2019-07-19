{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.Temperature.HTS221.RegTypes where

import Ivory.Language

[ivory|
 bitdata Power :: Bit
   = powerDown  as 0b0
   | powerUp    as 0b1

 bitdata BDU :: Bit
   = bduContinuos as 0b0
   | bduBlock     as 0b1

 bitdata Rate :: Bits 2
   = rateOneShot    as 0b00
   | rate1Hz        as 0b01
   | rate7Hz        as 0b10
   | rate12Point5Hz as 0b11

 bitdata AVGT :: Bits 3
   = avgt2   as 0b000
   | avgt4   as 0b001
   | avgt8   as 0b010
   | avgt16  as 0b011 -- default
   | avgt32  as 0b100
   | avgt64  as 0b101
   | avgt128 as 0b110
   | avgt256 as 0b111

 bitdata AVGH :: Bits 3
   = avgh4   as 0b000
   | avgh8   as 0b001
   | avgh16  as 0b010
   | avgh32  as 0b011 -- default
   | avgh64  as 0b100
   | avgh125 as 0b101
   | avgh256 as 0b110
   | avgh512 as 0b111

 bitdata PPOD :: Bit
   = ppOdPushPull  as 0b0 -- default
   | ppOdOpenDrain as 0b1

 bitdata DRDY :: Bit
   = drdyActiveHigh  as 0b0 -- default
   | drdyActiveLow   as 0b1
|]
