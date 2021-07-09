{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.Drivers.Encoder.AS5047.Types where

import Ivory.Language
import Ivory.Tower

[ivory|
struct ams_diag
  { magfield_low        :: Stored IBool
  ; magfield_high       :: Stored IBool
  ; cordic_overflow     :: Stored IBool
  ; offset_compensation :: Stored IBool
  ; agc_value           :: Stored Uint8
  }
|]

amsTypes :: Module
amsTypes = package "amsTypes" $ do
  defStruct (Proxy :: Proxy "ams_diag")

amsTowerDeps :: Tower e ()
amsTowerDeps = do
  towerDepends amsTypes
  towerModule amsTypes

