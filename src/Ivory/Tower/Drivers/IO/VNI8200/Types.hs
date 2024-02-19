{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.Drivers.IO.VNI8200.Types where

import Ivory.Language
import Ivory.Stdlib

[ivory|
 struct vni_status
   { vni_channel_over_temperature :: Array 8 (Stored IBool) -- ^ Per-channel over-temperature faults
   ; vni_dc_dc_ok                 :: Stored IBool -- ^ DC-DC regulator okay
   ; vni_over_temperature_warning :: Stored IBool -- ^ Over temperature warning
   ; vni_power_not_good           :: Stored IBool -- ^ Power no good
   ; vni_parity_error             :: Stored IBool -- ^ Parity check failed
   }
|]

vniTypes :: Module
vniTypes = package "vniTypes" $ do
  defStruct (Proxy :: Proxy "vni_status")

-- | Check if vni_status contains any faults
vniHasFaults
  :: (GetAlloc eff ~ 'Scope s)
  => ConstRef cs ('Struct "vni_status")
  -> Ivory eff IBool
vniHasFaults status = do
  dc_dc_ok <- status ~>* vni_dc_dc_ok
  over_temperature_warning <- status ~>* vni_over_temperature_warning
  power_not_good <- status ~>* vni_power_not_good
  parity_error <- status ~>* vni_parity_error
  someChanOVT <- local $ ival false
  arrayMap $ \ix -> do
    chanOVT <- deref (status ~> vni_channel_over_temperature ! ix)
    when
      (chanOVT)
      (store someChanOVT true)
  channel_is_over_temperature <- deref someChanOVT

  pure
    $   iNot dc_dc_ok
    .|| over_temperature_warning
    .|| power_not_good
    .|| parity_error
    .|| channel_is_over_temperature
