{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.Drivers.IO.CLT01.Types where

import Ivory.Language
import Ivory.Stdlib

[ivory|
 struct clt_status
   { clt_channel_status         :: Array 8 (Stored IBool)
   ; clt_under_voltage_alarm    :: Stored IBool
   ; clt_over_temperature_alarm :: Stored IBool
   ; clt_parity_error           :: Stored IBool
   }
|]

cltTypes :: Module
cltTypes = package "cltTypes" $ do
  defStruct (Proxy :: Proxy "clt_status")

-- | Check if clt_status contains any faults
cltHasFaults
  :: ConstRef s ('Struct "clt_status")
  -> Ivory eff IBool
cltHasFaults status = do
  under_voltage_alarm <- status ~>* clt_under_voltage_alarm
  over_temperature_alarm <- status ~>* clt_over_temperature_alarm
  parity_error <- status ~>* clt_parity_error
  pure
    $   under_voltage_alarm
    .|| over_temperature_alarm
    .|| parity_error
