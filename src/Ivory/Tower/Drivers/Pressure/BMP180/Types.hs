{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.Drivers.Pressure.BMP180.Types where

import Ivory.Language

-- Temperature in Celsius, pressure Pascal
[ivory|
 struct bmp_sample
   { bmp_sample_temperature :: Stored IFloat
   ; bmp_sample_pressure    :: Stored IFloat
   }
|]

bmp180Types :: Module
bmp180Types = package "bmp180Types" $ do
  defStruct (Proxy :: Proxy "bmp_sample")
