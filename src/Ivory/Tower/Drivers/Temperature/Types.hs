{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.Drivers.Temperature.Types where

import Ivory.Language
import Ivory.Tower.Types.Time

-- Temperature / humidity sample struct types
-- * temperature units are in degrees Celsius
-- * humidity is a relative humidity percentage

[ivory|
 struct sample_th
  { sample_th_temperature :: Stored IFloat
  ; sample_th_humidity    :: Stored IFloat
  ; sample_th_time        :: Stored ITime
  }

 struct sample_t
  { sample_t_temperature :: Stored IFloat
  ; sample_t_time        :: Stored ITime
  }

 struct sample_h
  { sample_h_humidity    :: Stored IFloat
  ; sample_h_time        :: Stored ITime
  }
|]
