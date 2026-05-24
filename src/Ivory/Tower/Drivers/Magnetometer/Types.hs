{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.Drivers.Magnetometer.Types where

import Ivory.Language

-- Values are in μT (microtesla)
[ivory|
 struct magnetometer_sample
   { x          :: Stored IFloat
   ; y          :: Stored IFloat
   ; z          :: Stored IFloat
   ; initfail   :: Stored IBool
   ; samplefail :: Stored IBool
   }
|]

magnetometerTypes :: Module
magnetometerTypes = package "magnetometerTypes" $ do
  defStruct (Proxy :: Proxy "magnetometer_sample")
