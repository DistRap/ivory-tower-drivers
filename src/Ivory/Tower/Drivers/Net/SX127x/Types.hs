{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.Drivers.Net.SX127x.Types where

import Ivory.Language

type SXArray = 'Array 128 ('Stored Uint8)

[ivory|
 struct radio_request
  { radio_tx_buf :: SXArray
  ; radio_tx_len :: Stored (Ix 128)
  }

 struct radio_listen
  { radio_listen_continuous :: Stored IBool
  ; radio_listen_frequency :: Stored Uint32
  }

 struct radio_result
  { radio_rx_buf  :: SXArray
  ; radio_rx_len  :: Stored (Ix 128)
  ; radio_rx_rssi :: Stored IFloat
  ; radio_rx_snr  :: Stored IFloat
  }
|]

radioDriverTypes :: Module
radioDriverTypes = package "radioDriverTypes" $ do
  defStruct (Proxy :: Proxy "radio_request")
  defStruct (Proxy :: Proxy "radio_listen")
  defStruct (Proxy :: Proxy "radio_result")
