{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.Net.RN2483.Types where

import Ivory.Language

newtype RadioCommandMode = RadioCommandMode Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

modeUnconfirmed, modeConfirmed, modeRaw :: RadioCommandMode
[modeRaw, modeConfirmed, modeUnconfirmed]
  = map (RadioCommandMode . fromInteger) [0..2]

data FrequencyPlan =
    Default
  | TTNEurope868
  | SingleChannel Int
  deriving (Eq, Show)

-- Over The Air Activation
-- or Activation By Personalisation
data RadioActivationMode = OTAA | ABP
  deriving (Eq, Show)

data RadioActivation =
  OTAAConfig {
    otaaAppEUI :: String
  , otaaAppKey :: String
  }
  | ABPConfig {
    abpDevAddr           :: String
  , abpNetworkSessionKey :: String
  , abpAppSessionKey     :: String
  } deriving (Eq, Show)

data RadioConfig = RadioConfig {
    rcPower           :: Int -- 1 to 5 for Europe (1 is max power @ 14dB)
  , rcDataRate        :: Int -- 0 to 5 (0 is SF12BW125, 5 is SF7BW125)
  , rcCodingRate      :: Int -- 1 to 4 (4/(4 + rcCodingRate) ~> e.g. rcCodingRate = 1 results in CR4/5)
  , rcBandWidth       :: Int
  , rcADR             :: Bool -- Adaptive data rate
  , rcFrequencyPlan   :: FrequencyPlan
  , rcDevEUI          :: Maybe String
  , rcActivationMode  :: RadioActivationMode
  , rcActivation      :: Maybe RadioActivation
  , rcRetries         :: Int
  , rcConfirmed       :: Bool
  , rcAutoReply       :: Bool
  } deriving (Eq, Show)

defaultRadioConfig :: RadioConfig
defaultRadioConfig = RadioConfig {
    rcPower          = 1    -- max power
  , rcDataRate       = 0    -- SF12
  , rcCodingRate     = 1    -- 4/5
  , rcBandWidth      = 125
  , rcADR            = True
  , rcDevEUI         = Nothing
  , rcActivation     = Nothing
  , rcActivationMode = OTAA
  , rcFrequencyPlan  = TTNEurope868
  , rcRetries        = 1
  , rcConfirmed      = False
  , rcAutoReply      = True
  }

-- Produce list of commands for RN2483 according to RadioConfig
rn2483Configure :: RadioConfig -> [String]
rn2483Configure RadioConfig{..} = [
    "mac set pwridx " ++ show rcPower
  , "mac set dr "     ++ show rcDataRate
  , "mac set adr "    ++ boolToOnOff rcADR
  , "radio set cr 4/" ++ show (4 + rcCodingRate)
  , "radio set bw "   ++ show rcBandWidth
  , "mac set retx "   ++ show rcRetries   -- retransmission attempts
  , "mac set ar "     ++ boolToOnOff rcAutoReply
  , "mac get deveui"
  ] ++ maybe [] (\eui -> [ "mac set deveui " ++ eui ]) rcDevEUI
    ++ rnFreqPlan rcActivationMode rcFrequencyPlan

rnActivate :: RadioActivation -> [String]
rnActivate OTAAConfig{..} = [
    "mac set appeui " ++ otaaAppEUI
  , "mac set appkey " ++ otaaAppKey
  ]
rnActivate ABPConfig{..} = [
    "mac set devaddr " ++ abpDevAddr
  , "mac set nwkskey " ++ abpNetworkSessionKey
  , "mac set appskey " ++ abpAppSessionKey
  ]

rnActivationMode :: RadioActivationMode -> String
rnActivationMode OTAA = "otaa"
rnActivationMode ABP  = "abp"

boolToOnOff :: Bool -> String
boolToOnOff True = "on"
boolToOnOff False = "off"

chanEnabled :: Int -> Bool -> String
chanEnabled chan state = unwords [
    "mac set ch status"
  , show chan
  , boolToOnOff state]
  where

chanEnable :: Int -> String
chanEnable chan = chanEnabled chan True

chanDisable :: Int -> String
chanDisable chan = chanEnabled chan False

-- Set channel frequency
-- from 863000000 to 870000000 or
-- from 433050000 to 434790000, in Hz.
chanFreqHz :: Int -> Int -> String
chanFreqHz chan freq =
  unwords ["mac set ch freq", show chan, show freq]

-- Set channel duty cycle, from 0 to 100%
chanDutyCycle :: Int -> Float -> String
chanDutyCycle chan duty =
  unwords ["mac set ch dcycle", show chan, show $ dutyFromPercentage duty]
  where
    dutyFromPercentage :: Float -> Int
    dutyFromPercentage p = floor $ 100 / p - 1

-- Set channel data rate range, from 0 to 7
chanDataRateRange :: Int -> Int -> Int -> String
chanDataRateRange chan minRange maxRange =
  unwords ["mac set ch drrange", show chan, show minRange, show maxRange]

-- Configure frequency plan
-- see https://www.thethingsnetwork.org/docs/lorawan/frequency-plans.html
rnFreqPlan :: RadioActivationMode -> FrequencyPlan -> [String]
rnFreqPlan _ Default = []
-- see TTN frequency plan
rnFreqPlan mode TTNEurope868 =
  [ chanDutyCycle c (1 / (fromIntegral $ length chans)) | c <- chans]
  ++ concat [
    [ chanDataRateRange c 0 5
    , chanFreqHz c freq
    , chanEnable c
    ] | c <- nChans ]
  ++ [ chanDataRateRange 1 0 6 ] -- Channel 1@868.3Mhz also allows DR6 (SF7BW250)
  ++ abpDownlink mode
  where
  chans = [0..7]
  nChans = [3..7] -- non-default chans, we only configure freq/data rate for these
  freq = 867100000
  -- non-standard TTN rx2 window
  abpDownlink OTAA = []
  abpDownlink ABP  = [ "mac set rx2 3 869525000" ]
-- only channel 3 with specified frequency
-- not LoraWAN compliant, for testing only
rnFreqPlan _ (SingleChannel freq) =
  [ chanFreqHz 3 freq
  , chanDataRateRange 3 0 5
  , chanDutyCycle 3 0.125
  , chanEnable 3
  ]
  ++ [ chanDisable c | c <- [0..2] ]

checkRadioConfig :: Monad m => RadioConfig -> m ()
checkRadioConfig RadioConfig{..} = do
  check "rcPower"           (rcPower >= 1 && rcPower <= 5)
  check "rcDataRate"        (rcDataRate `elem` [0..6])
  check "rcCodingRate"      (rcCodingRate `elem` [1..4])
  check "rcBandWidth"       (rcBandWidth `elem` [125, 250, 500])
  case rcDevEUI of
    Nothing -> return ()
    Just eui -> check "radio deveui" (length eui == 16)
  case rcActivation of
    Nothing -> return ()
    Just act -> checkActivation act
  where
    check _ True = return ()
    check name False = error $ "Radio config value " ++ name ++ " is out of range"

    checkActivation :: Monad m => RadioActivation -> m ()
    checkActivation OTAAConfig{..} = do
      check "otaa eui" (length otaaAppEUI == 16)
      check "otaa key" (length otaaAppKey == 32)
    checkActivation ABPConfig{..} = do
      check "abp devaddr" (length abpDevAddr == 8)
      check "abp nwkskey" (length abpNetworkSessionKey == 32)
      check "abp appskey" (length abpAppSessionKey == 32)
