{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.IO.CLT01.Regs where

import Ivory.Language

[ivory|
 bitdata CLTStatus :: Bits 8 = clt_status
   { clt_voltage_ok     :: Bit    -- !UVA (0 when under voltage alarm)
   , clt_temperature_ok :: Bit    -- !OVA (0 when over temperature)
   , clt_parity         :: Bits 4 -- PC1 .. PC4
   , clt_stopbits       :: Bits 2 -- always 0b01
   }
|]
