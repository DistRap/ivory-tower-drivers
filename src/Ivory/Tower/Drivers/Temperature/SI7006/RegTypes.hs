{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.Temperature.SI7006.RegTypes where

import Ivory.Language

[ivory|
 bitdata VDDS :: Bit
   = powerOk  as 0b0
   | powerLow as 0b1

 bitdata HEATER :: Bit
   = heaterOff as 0b0
   | heaterOn  as 0b1
|]
