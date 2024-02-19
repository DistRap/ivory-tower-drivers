{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.IO.VNI8200.Regs where

import Ivory.Language
-- import LDrive.DRV8301.RegTypes
-- import LDrive.Ivory.Types.DrvFault

[ivory|
 bitdata VNIStatus :: Bits 8 = vni_status
   { vni_fb_ok               :: Bit -- DC-DC regulator power good
   , vni_temperature_warning :: Bit -- !TWARN (0 = good)
   , vni_parity_fail         :: Bit -- PC Parity check fail of last message
   , vni_not_power_good      :: Bit -- !PG not power good (0 = good)
   , vni_p0                  :: Bit
   , vni_p1                  :: Bit
   , vni_p2                  :: Bit
   , vni_np0                 :: Bit
   }
|]
