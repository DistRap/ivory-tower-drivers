{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.Drivers.Net.SX127x.Types where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Drivers.Net.LoRa

type SXArray = 'Array 128 ('Stored Uint8)

[ivory|
 struct radio_link
  { radio_link_frequency   :: Stored Uint32
  ; radio_link_iq_invert   :: Stored IBool
  ; radio_link_bandwidth   :: Stored Bandwidth
  ; radio_link_spreading   :: Stored SpreadingFactor
  ; radio_link_coding_rate :: Stored CodingRate
  }

 struct radio_request
  { radio_tx_buf :: SXArray
  ; radio_tx_len :: Stored (Ix 128)
  ; radio_tx_conf :: Struct radio_link
  }

 struct radio_listen
  { radio_listen_continuous :: Stored IBool
  ; radio_listen_conf       :: Struct radio_link
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
  defStruct (Proxy :: Proxy "radio_link")
  defStruct (Proxy :: Proxy "radio_request")
  defStruct (Proxy :: Proxy "radio_listen")
  defStruct (Proxy :: Proxy "radio_result")

data LNAGain =
    LNAGainAutomatic
  | LNAGainStatic {
      lnaGain    :: Int
    , lnaBoostHF :: Bool
    }
  deriving (Eq, Show)

data Time a => SXConfig a = SXConfig {
    sx_crc              :: Bool
  , sx_implicit_header  :: Bool
  , sx_lna              :: LNAGain
  , sx_output_pa_boost  :: Bool
  , sx_output_max_power :: Int -- [0 .. 7]
  , sx_output_power     :: Int -- [0 .. 15] which results in
                               --   +2 .. +17 dBm for PA_BOOST
                               --   -4 .. +15 dBm for RFO, depending on max_power
  , sx_sync_word        :: Int
  , sx_polling_period   :: Maybe a
  , sx_isr              :: Maybe (ChanOutput ('Stored IBool))
  }

defaultConfig :: SXConfig Milliseconds
defaultConfig = SXConfig {
    sx_crc              = True
  , sx_implicit_header  = True
  , sx_lna              = LNAGainAutomatic
  , sx_output_pa_boost  = True
  , sx_output_max_power = 4
  , sx_output_power     = 15
  , sx_sync_word        = privateNetSyncWord
  , sx_polling_period   = Just $ Milliseconds 100
  , sx_isr              = Nothing
  }
