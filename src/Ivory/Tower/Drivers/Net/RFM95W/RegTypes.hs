{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.Net.RFM95W.RegTypes where

import Ivory.Language

[ivory|
 bitdata RFMRW :: Bit
   = rfm_read  as 0b1
   | rfm_write as 0b0

 bitdata RFMAddr :: Bits 7
   = rfm_fifo               as 0x00
   | rfm_op_mode            as 0x01
   | rfm_fr_msb             as 0x06
   | rfm_fr_mid             as 0x07
   | rfm_fr_lsb             as 0x08
   | rfm_pa_config          as 0x09
   | rfm_pa_ramp            as 0x0a
   | rfm_ocp                as 0x0b
   | rfm_lna                as 0x0c
   | rfm_fifo_addr_ptr      as 0x0d
   | rfm_fifo_tx_base_addr  as 0x0e
   | rfm_fifo_rx_base_addr  as 0x0f
   | rfm_fifo_rx_curr_addr  as 0x10
   | rfm_irq_flags_mask     as 0x11
   | rfm_irq_flags          as 0x12
   | rfm_rx_nb_bytes        as 0x13
   | rfm_rx_header_cnt_msb  as 0x14
   | rfm_rx_packet_cnt_msb  as 0x16
   | rfm_modem_stat         as 0x18
   | rfm_pkt_snr_value      as 0x19
   | rfm_pkt_rssi_value     as 0x1a
   | rfm_rssi_value         as 0x1b
   | rfm_hop_channel        as 0x1c
   | rfm_modem_config_1     as 0x1d
   | rfm_modem_config_2     as 0x1e
   | rfm_symb_timeout_lsb   as 0x1f
   | rfm_preamble_msb       as 0x20
   | rfm_payload_length     as 0x22
   | rfm_max_payload_length as 0x23
   | rfm_hop_period         as 0x24
   | rfm_fifo_rx_byte_addr  as 0x25
   | rfm_modem_config_3     as 0x26

 bitdata Mode :: Bits 3
   = rfm_mode_sleep        as 0b000
   | rfm_mode_stdby        as 0b001
   | rfm_mode_fstx         as 0b010 -- Frequency synthesis TX
   | rfm_mode_tx           as 0b011
   | rfm_mode_fsrx         as 0b100 -- Frequency synthesis RX
   | rfm_mode_rxcontinuous as 0b101
   | rfm_mode_rxsingle     as 0b110
   | rfm_mode_cad          as 0b111 -- Channel activity detection

 bitdata PaRamp :: Bits 4
   = rfm_pa_ramp_3ms4      as 0b0000
   | rfm_pa_ramp_2ms       as 0b0001
   | rfm_pa_ramp_1ms       as 0b0010
   | rfm_pa_ramp_500us     as 0b0011
   | rfm_pa_ramp_250us     as 0b0100
   | rfm_pa_ramp_125us     as 0b0101
   | rfm_pa_ramp_100us     as 0b0110
   | rfm_pa_ramp_62us      as 0b0111
   | rfm_pa_ramp_50us      as 0b1000
   | rfm_pa_ramp_40us      as 0b1001
   | rfm_pa_ramp_31us      as 0b1010
   | rfm_pa_ramp_25us      as 0b1011
   | rfm_pa_ramp_20us      as 0b1100
   | rfm_pa_ramp_15us      as 0b1101
   | rfm_pa_ramp_12us      as 0b1110
   | rfm_pa_ramp_10us      as 0b1111

 bitdata LnaGain :: Bits 3
   = rfm_lna_gain_g1       as 0b001 -- maximum gain
   | rfm_lna_gain_g2       as 0b010
   | rfm_lna_gain_g3       as 0b011
   | rfm_lna_gain_g4       as 0b100
   | rfm_lna_gain_g5       as 0b101
   | rfm_lna_gain_g6       as 0b110 -- minimum gain

 bitdata Bandwidth :: Bits 4
   = rfm_bandwidth_7_8_kHz   as 0b0000
   | rfm_bandwidth_10_4_kHz  as 0b0001
   | rfm_bandwidth_15_6_kHz  as 0b0010
   | rfm_bandwidth_20_8_kHz  as 0b0011
   | rfm_bandwidth_31_25_kHz as 0b0100
   | rfm_bandwidth_41_7_kHz  as 0b0101
   | rfm_bandwidth_62_5_kHz  as 0b0110
   | rfm_bandwidth_125_kHz   as 0b0111
   | rfm_bandwidth_250_kHz   as 0b1000
   | rfm_bandwidth_500_kHz   as 0b1001

 bitdata SpreadingFactor :: Bits 4
   = rfm_spreadingfactor_6   as 6
   | rfm_spreadingfactor_7   as 7
   | rfm_spreadingfactor_8   as 8
   | rfm_spreadingfactor_9   as 9
   | rfm_spreadingfactor_10  as 10
   | rfm_spreadingfactor_11  as 11
   | rfm_spreadingfactor_12  as 12

|]
