{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.OS.Posix.Tower
import Ivory.OS.Posix.Tower.IO
import Ivory.OS.Posix.Tower.Serial

import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types

import Ivory.Tower.Drivers.Net.RN2483

app :: Tower e ()
app = do
  uartTowerDeps

  (buffered_ostream_stdout, istream_stdin) <- serialIO

  ostream_stdout <- uartUnbuffer (
    buffered_ostream_stdout :: BackpressureTransmit UARTBuffer ('Stored IBool))

  [fd,_] <- getFDs  ["/dev/ttyUSB0"]
  (buffered_ostream, istream) <- serialIOFD fd
  ostream <- uartUnbuffer (
    buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))

  --rn2483Tower ostream istream
  monitor "bridge" $ do
    handler istream "bridge_istream" $ do
      out <- emitter ostream_stdout 1
      callback $ \val ->
        emit out val

  return ()

main :: IO ()
main = compileTowerPosix (const $ return ()) app
