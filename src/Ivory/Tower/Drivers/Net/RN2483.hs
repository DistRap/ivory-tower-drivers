{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Drivers.Net.RN2483 where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Base.GPIO
import Ivory.Tower.Base.UART

import Ivory.BSP.STM32.Peripheral.GPIO

rn2483init :: [String]
rn2483init = [
    "radio set pwr 15"
  , "radio set sf sf12"
  , "radio set cr 4/5"
  , "radio set wdt 15000"
  , "radio set iqi off"
  , "radio set bw 125"
  ]

rn2483 :: forall buf e . (IvoryString buf)
       => ChanInput ('Stored Uint8)
       -> ChanOutput buf
       -> ChanOutput ('Stored ITime)
       -> GPIOPin
       -> Proxy buf
       -> Tower e ( ChanOutput ('Stored IBool)
                  , ChanOutput ('Stored IBool)
                  , ChanOutput ('Stored IBool)
                  , ChanInput buf)
rn2483 ostream istream initChan rstPin _proxybuf = do
  ready <- channel
  acceptChan <- channel
  txdone <- channel
  cmdChan <- channel

  monitor "rn2483" $ do
    handler systemInit "rn2483init" $ do
      callback $ const $ do
        pinOutSpeed rstPin gpio_speed_2mhz
        pinLow rstPin

    received <- stateInit "rn2483_received" (ival (0 :: Uint32))
    (locbuf :: Ref 'Global buf) <- state "rn2483_buf"

    (tmp :: Ref 'Global buf) <- state "rn2483_tmp"

    stateOperational <- stateInit "rn2483_operational" (ival false)

    handler (snd cmdChan) "rn2383cmd" $ do
      o <- emitter ostream 32
      callback $ \cmd -> do
        puts o "mac tx cnf 1 "

        len <- cmd ~>* stringLengthL

        arrayMap $ \ix -> do
          unless (fromIx ix >=? len) $ do
            c <- deref (cmd ~> stringDataL ! ix)
            putHex o c

        puts o "\r\n"

    coroutineHandler initChan istream "rn2483i" $ do
      o <- emitter ostream 32
      rdy <- emitter (fst ready) 1
      accept <- emitter (fst acceptChan) 1
      txd <- emitter (fst txdone) 1
      return $ CoroutineBody $ \ yield -> do

        let isPrefixOf p x = isPrefixOfBuf p x (Proxy :: Proxy buf)
        let rpc cmd = do
               puts o $ cmd ++ "\r\n"
               res <- yield
               refCopy tmp res
               gotOk <- "ok" `isPrefixOf` res
               assert $ gotOk
               return ()

        pinHigh rstPin

        input <- yield
        res <- "RN2483" `isPrefixOf` input
        when res $ do
          flip mapM_ rn2483init $ \cmd -> rpc cmd

          store stateOperational true

          emitV rdy true

          rpc "mac join otaa"
          joinres <- yield
          refCopy tmp joinres

          accepted <- "accept" `isPrefixOf` joinres
          denied <- "denied" `isPrefixOf` joinres

          cond_
            [ accepted ==> do
                emitV accept true
                --rpc "mac tx cnf 1 48"

                -- msg from handler above
                res <- yield
                refCopy tmp res
                gotOk <- "ok" `isPrefixOf` res
                assert $ gotOk

                let expect x act = do
                      result <- yield
                      match <- x `isPrefixOf` result
                      when match $ act

                expect "mac_tx_ok" $ emitV txd true

            , denied ==> emitV accept false
            ]

          forever $ do
            _ <- yield
            refCopy locbuf input

          received += 1
  return (snd ready, snd acceptChan, snd txdone, fst cmdChan)
