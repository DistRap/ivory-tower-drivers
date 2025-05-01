{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.DAC.DACx1408.RegTypes where

import Ivory.Language

[ivory|
  bitdata ToggleMode :: Bits 2
    = toggle_mode_disabled as 0b00
    | toggle_mode_toggle0  as 0b01
    | toggle_mode_toggle1  as 0b10
    | toggle_mode_toggle2  as 0b11

  bitdata SoftReset :: Bits 4
    = soft_reset_trigger as 0b1010

  bitdata RangeVolts :: Bits 4
    = range_0to5volts                  as 0b0000
    | range_0to10volts                 as 0b0001
    | range_0to20volts                 as 0b0010
    | range_0to40volts                 as 0b0100
    | range_minus5to5volts             as 0b1001
    | range_minus10to10volts           as 0b1010
    | range_minus20to20volts           as 0b1100
    | range_minus2point5to2point5volts as 0b1110
|]
