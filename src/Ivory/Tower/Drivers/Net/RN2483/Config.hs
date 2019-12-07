{-# LANGUAGE RecordWildCards #-}

module Ivory.Tower.Drivers.Net.RN2483.Config where

import Ivory.Tower.Drivers.Net.RN2483.Types

import Ivory.Tower.Config
import Data.Char (toLower)

radioConfigParser :: ConfigParser RadioConfig
radioConfigParser = subsection "radio" $ do
  let RadioConfig{..} = defaultRadioConfig

  power <- subsection "power"              int    <|> pure rcPower
  dr    <- subsection "data-rate"          int    <|> pure rcDataRate
  cr    <- subsection "coding-rate"        int    <|> pure rcCodingRate
  bw    <- subsection "bandwidth"          int    <|> pure rcBandWidth
  adr   <- subsection "adaptive-data-rate" bool   <|> pure rcADR
  fpMaybe <- optional $ subsection "frequency-plan" string

  let fp = case fmap (map toLower) fpMaybe of
             Nothing       -> Default
             Just "europe" -> TTNEurope868
             Just x        -> error $ "Unknown frequency plan " ++ x

  deveui <- optional $ subsection "device-eui" string

  amMaybe <- optional $ subsection "activation-mode" string
  let am = case fmap (map toLower) amMaybe of
             Nothing     -> OTAA
             Just "otaa" -> OTAA
             Just "abp"  -> ABP
             Just x      -> error $ "Unknown activation mode " ++ x ++ " use one of otaa/abp"

  abp <- optional (subsection "abp" abpParser)
  otaa <- optional (subsection "otaa" otaaParser)
  let act = case (am, amMaybe, abp, otaa) of
              -- if activation-mode is undefined but we have abp or otaa params set 
              -- use these (prioritizing otaa)
              (_, Nothing, _, Just o) -> Just o
              (_, Nothing, Just a, _) -> Just a
              -- if activation-mode is set use respective config section
              (OTAA, _, _, Just o) -> Just o
              (ABP,  _, Just a, _) -> Just a
              -- no activation sections
              _ -> Nothing

  retries <- subsection "retries" int <|> pure rcRetries
  cnf     <- subsection "confirmed" bool <|> pure rcConfirmed
  ar      <- subsection "auto-reply" bool <|> pure rcAutoReply

  return RadioConfig {
      rcPower           = power
    , rcDataRate        = dr
    , rcCodingRate      = cr
    , rcBandWidth       = bw
    , rcADR             = adr
    , rcFrequencyPlan   = fp
    , rcDevEUI          = deveui
    , rcActivationMode  = am
    , rcActivation      = act
    , rcRetries         = retries
    , rcConfirmed       = cnf
    , rcAutoReply       = ar
    }

activationParser :: ConfigParser RadioActivation
activationParser = subsection "otaa" otaaParser <|> subsection "abp" abpParser

abpParser :: ConfigParser RadioActivation
abpParser = do
  devaddr <- subsection "devaddr" string
  nwkskey <- subsection "nwkskey" string
  appskey <- subsection "appskey" string
  return $ ABPConfig {
      abpDevAddr           = devaddr
    , abpNetworkSessionKey = nwkskey
    , abpAppSessionKey     = appskey
    }

otaaParser :: ConfigParser RadioActivation
otaaParser = do
  eui <- subsection "eui" string
  key <- subsection "key" string
  return $ OTAAConfig {
      otaaAppEUI = eui
    , otaaAppKey = key
    }
