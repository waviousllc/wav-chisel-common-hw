package wav.common

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.ChiselStage


class WavScanBundle(val numChains: Int = 1) extends Bundle{
  val mode            = Input (Bool())
  val asyncrst_ctrl   = Input (Bool())
  val clk             = Input (Bool())
  val shift           = Input (Bool())
  val in              = Input (UInt(numChains.W))
  val out             = Output(UInt(numChains.W))
  
  
  def connectScan(b: WavScanBundle){
    mode          := b.mode
    asyncrst_ctrl := b.asyncrst_ctrl
    clk           := b.clk
    in            := b.in
    shift         := b.shift
    dontTouch(out)
    dontTouch(b.out)
  }
}
