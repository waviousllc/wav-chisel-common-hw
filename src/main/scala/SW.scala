package wav.common

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.ChiselStage

//import chisel3.experimental._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.ahb._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.subsystem.CrossingWrapper
import freechips.rocketchip.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.unittest._
import freechips.rocketchip.devices.tilelink._


class WavBitfield(){}

/*
.rst_start
Software Register Creations
------------------------------
To facilitate easy register creation several Register classes are supplied. These register classes encapsulate 
various amounts of logic reducing the need for a designer to implement various features independently for
each instance of a register class. These classes also give the designer confidence that they are producing
the same circuit for each instance.

The basic register component created is a RocketChip RegField object, in which addtional logic is then added on top.
 
The following register types are currently supported

* RW - Read Write Registers
* RO - Read Only Registers


.rst_end
*/


/**
  *   Custom Software Register Generation
  *  
  *   Each Register is given a class and accompanying object. The class is basically for creating a list/Seq and is the 
  *   main component a user should call. The object (with "_Reg" suffix) is used to actually create the register and is called
  *   by the WavSWReg. WavSWReg calls this inorder to pass the register Name and description. This saves the user from having
  *   to also pass the name/desc for the register on EVERY bitfield declaration.
  *
  */

case class WavRW [T <: Data](
  connection : T,
  reset      : T,
  name       : String,
  desc       : String = "",
  corescan   : Option[(Bool, T)] = None,
  iddq       : Option[(Bool, T)] = None,
  highz      : Option[(Bool, T)] = None,
  bscan      : Option[(Bool, T)] = None,
  bflop      : Option[WavBSR] = None
) extends WavBitfield{}

object WavRWReg{
  
  def apply[T <: Data](
    connection  : T, 
    reset       : T, 
    name        : String, 
    desc        : String = "", 
    regName     : String = "", 
    regDesc     : String = "",
    corescan    : Option[(Bool, T)] = None,
    iddq        : Option[(Bool, T)] = None,
    highz       : Option[(Bool, T)] = None,
    bscan       : Option[(Bool, T)] = None,
    bflop       : Option[WavBSR] = None)(implicit p: Parameters): RegField = {

    val reg = RegInit(connection.cloneType, reset)
        
    reg.suggestName("swi_" + name)
    
    
    //connection := reg
    val corescan_sig = iddq match{
      case Some(x) => WavClockMux(x._1, x._2, reg).suggestName("corescan_dft_mux_"+name)
      case None    => reg
    }
    
    val iddq_sig = iddq match{
      case Some(x) => WavClockMux(x._1, x._2, corescan_sig).suggestName("iddq_dft_mux_"+name)
      case None    => corescan_sig
    }
    val hiz_sig = highz match{
      case Some(x) => WavClockMux(x._1, x._2, iddq_sig).suggestName("hiz_dft_mux_"+name)
      case None    => iddq_sig
    }
    val bscan_sig = bscan match{
      case Some(x) => WavClockMux(x._1, x._2, hiz_sig).suggestName("bscan_dft_mux_"+name)
      case None    => hiz_sig
    }
    
    val bflop_sig = bflop match{
      case Some(bsr) => {
        bsr.io.pi  := bscan_sig
        bsr.io.po
      }
      case None    => bscan_sig
    }
    
    
    
    connection := bflop_sig
    
    
    //litValue returns the Bigint value
    val rf = RegField(reg.getWidth, reg.asUInt, RegFieldDesc(name, desc, access=RegFieldAccessType.RW , reset=Some(reset.litValue), group=Some(regName), groupDesc=Some(regDesc)))
    rf
  }
}


case class WavW1C [T <: Data](
  in         : Bool,
  out        : Bool,
  name       : String,
  desc       : String = ""
) extends WavBitfield{}

object WavW1CReg{
  
  def apply[T <: Data](
    in          : Bool, 
    out         : Bool, 
    name        : String, 
    desc        : String = "", 
    regName     : String = "", 
    regDesc     : String = "")(implicit p: Parameters): RegField = {

    //W1C register
    val w1c = RegInit(in.cloneType, 0.U)
    w1c.suggestName(name + "_w1c")
    
    val ff2 = WavDemetReset(in)
    ff2.suggestName(name+"_w1c_ff2")
    val ff3 = RegNext(ff2, 0.U)
    ff3.suggestName(name+"_w1c_ff3")
    
    val set = ff2 & ~ff3
    set.suggestName(name+"_w1c_set")
    
    out := w1c
    //See about checking the wrType as well during annotation
    val rf = RegField.w1ToClear(w1c.getWidth, w1c.asUInt, set, Some(RegFieldDesc(name, desc, access=RegFieldAccessType.RW , reset=Some(0), group=Some(regName), groupDesc=Some(regDesc))))
    rf
  }
}


/**
  *   Similar to the WavRWReg but is only for Read-Only type of registers.
  *   Since there is no physical register the reset value is not given
  *   A user can add in a demet synchronizer to remove async crossing
  */
case class WavRO [T <: Data](
  connection : T,
  name       : String,
  desc       : String = "",
  demet      : Boolean = false,
  bflop      : Option[WavBSR] = None
) extends WavBitfield{}

object WavROReg{
  def apply[T <: Data](
    connection: T, 
    name      : String, 
    desc      : String = "", 
    demet     : Boolean = false, 
    regName   : String = "", 
    regDesc   : String = "",
    bflop     : Option[WavBSR] = None)(implicit p: Parameters): RegField = {
    
    if(demet) {
      val readval = WavDemetReset(connection)
      readval.suggestName("RORegDemet_" + name)
      
      val bflop_sig = bflop match{
        case Some(bsr) => bsr.io.pi  := readval
        case _         => Unit
      }
      
      val rf = RegField.r(connection.getWidth, readval.asUInt, RegFieldDesc(name, desc, access=RegFieldAccessType.R, reset=Some(0), volatile=true, group=Some(regName), groupDesc=Some(regDesc)))
      rf
    } else {
      val readval = Wire(connection.cloneType)
      readval := connection
      
      val bflop_sig = bflop match{
        case Some(bsr) => bsr.io.pi  := readval
        case _         => Unit
      }
      
      val rf = RegField.r(connection.getWidth, readval.asUInt, RegFieldDesc(name, desc, access=RegFieldAccessType.R, reset=Some(0), volatile=true, group=Some(regName), groupDesc=Some(regDesc)))
      rf
    }
  }
  
}



/**
  *   A reserved register field
  */
case class WavRSVD [T <: Data](
  width : Int
) extends WavBitfield{}

object WavRSVDReg{
  def apply[T <: Data](width : Int, regName: String = "", regDesc: String = "")(implicit p: Parameters): RegField = {
    
    val rf = RegField.r(width, 0.U, RegFieldDesc("Reserved", "Reserved", access=RegFieldAccessType.R, reset=Some(0), group=Some(regName), groupDesc=Some(regDesc)))
    rf
  }
}

/**
  *   For Bundles
  */
case class WavROBundle [T <: Bundle](
  b     : T,
  name  : String,
  desc  : String = "",
  demet : Boolean = false
) extends WavBitfield{}

object WavROBundleReg{
  def apply[T <: Bundle](b : T, name: String, desc: String = "", demet: Boolean = false, regName: String = "", regDesc: String = "")(implicit p: Parameters): Seq[RegField] = {
  
    var bfs = Seq.empty[RegField]
    
    for((fname, field) <- b.elements){
      val rf = WavROReg(field, name+"_"+fname, desc, demet, regName, regDesc)
      bfs = bfs :+ rf
    }
    bfs
  }
}


case class WavSWBundle [T <: Bundle](
  b     : T,
  name  : String,
  desc  : String = "",
  demet : Boolean = false
) extends WavBitfield{}

object WavSWBundleReg{
  def apply[T <: Bundle](b : T, name: String, desc: String = "", demet: Boolean = false, regName: String = "", regDesc: String = "")(implicit p: Parameters): Seq[RegField] = {
  
    var bfs = Seq.empty[RegField]
    
    for((fname, field) <- b.elements){
      //DataMirror.directionOf allows us to get the ActualDirection of the pin in the Bundle
      DataMirror.directionOf(field) match {
        case Direction.Input => {
          val rf = WavROReg(field, name+"_"+fname, desc, demet, regName, regDesc)
          bfs = bfs :+ rf
        }
        case Direction.Output => {
          val rf = WavRWReg(field, 0.U, name+"_"+fname, desc, regName, regDesc)
          bfs = bfs :+ rf
        }
      }
    }
    bfs
  }
}


/**
  *   Creates a muxed override register. This allows software to take control of a particular signal by setting the "_mux" register
  *   high. Once in this mode, the value of the signal is based on the software register defined here.
  *
  *   This method makes the assumption that you want the two bitfields to be next to each other (reg then mux). If you do not want this
  *   or can't do this (because the signal you want to control is >=32bits) you will have to create two separate RWReg. One for the mux
  *   and one for the SW control value
  *
  *
  *   val rx_reset_reg    = Wire(Bool())
  *   val rx_reset_mux    = Wire(Bool())
  *   val rx_reset        = WavClockMux (rx_reset_mux,  rx_reset_reg,   io.core.rx_reset)
  *   WavD2DRxRegs.CoreOverride   -> Seq(WavRWReg(rx_reset_reg,0.U,      "rx_reset",       "RX Reset"),
  *                                      WavRWReg(rx_reset_mux,0.U,      "rx_reset_mux",   "Mux override for rx_reset")),
  *
  *
  *   This method also has the nuance that it returns a Seq[RegField]. So you must concatenate this companion object with the higher leve
  *   Seq[RegField] that we normally use for register generation
  *   
  *   WavRWMuxReg(in=io.core.tx_en,     muxed=io.ana.tx_en,    reg_reset=false.B, mux_reset=false.B, "tx_en",    "Main TX enable") ++ Seq(<other regFields>)
  */

case class WavRWMux [T <: Data](
  in         : T,
  muxed      : T,
  reg_reset  : T,
  mux_reset  : Bool,
  name       : String,
  desc       : String = "",
  corescan   : Option[(Bool, T)] = None,
  iddq       : Option[(Bool, T)] = None,
  highz      : Option[(Bool, T)] = None,
  bscan      : Option[(Bool, T)] = None,
  bflop      : Option[WavBSR] = None
) extends WavBitfield{}

object WavRWMuxReg{
  
  def apply[T <: Data](
    in: T, 
    muxed : T, 
    reg_reset: T, 
    mux_reset: Bool, 
    name: String, 
    desc: String = "", 
    regName: String = "", 
    regDesc: String = "",
    corescan   : Option[(Bool, T)] = None,
    iddq       : Option[(Bool, T)] = None,
    highz      : Option[(Bool, T)] = None,
    bscan      : Option[(Bool, T)] = None,
    bflop      : Option[WavBSR] = None)(implicit p: Parameters): Seq[RegField] = {

    val reg = RegInit(muxed.cloneType, reg_reset)
    reg.suggestName("swi_" + name)
    
    val mux = RegInit(mux_reset)
    mux.suggestName("swi_" + name + "_mux")
    
    //muxed := WavClockMux(mux, reg, in)
    val muxed_pre = WavClockMux(mux, reg, in)
    
    
    val corescan_sig = iddq match{
      case Some(x) => WavClockMux(x._1, x._2, muxed_pre).suggestName("corescan_dft_mux_"+name)
      case None    => muxed_pre
    }
    
    val iddq_sig = iddq match{
      case Some(x) => WavClockMux(x._1, x._2, corescan_sig).suggestName("iddq_dft_mux_"+name)
      case None    => corescan_sig
    }
    val hiz_sig = highz match{
      case Some(x) => WavClockMux(x._1, x._2, iddq_sig).suggestName("hiz_dft_mux_"+name)
      case None    => iddq_sig
    }
    val bscan_sig = bscan match{
      case Some(x) => WavClockMux(x._1, x._2, hiz_sig).suggestName("bscan_dft_mux_"+name)
      case None    => hiz_sig
    }
    
    val bflop_sig = bflop match{
      case Some(bsr) => {
        bsr.io.pi  := bscan_sig
        bsr.io.po
      }
      case None    => bscan_sig
    }
    
    muxed := bflop_sig
    
    //litValue returns the Bigint value
    val rf_reg = RegField(reg.getWidth, reg.asUInt, RegFieldDesc(name,        desc, access=RegFieldAccessType.RW , reset=Some(reg_reset.litValue), group=Some(regName), groupDesc=Some(regDesc)))
    val rf_mux = RegField(1           , mux.asUInt, RegFieldDesc(name+"_mux", "Mux control for corresponding register", access=RegFieldAccessType.RW , reset=Some(mux_reset.litValue), group=Some(regName), groupDesc=Some(regDesc)))
    Seq(rf_reg, rf_mux)
  }
}

/**
  *   An individual "SW register" composing of one or more bitfields
  *
  */
object WavSWReg {
  def apply(regAddr: Int, regName: String, regDesc: String, bitfields: WavBitfield*)(implicit p: Parameters): RegField.Map ={
    
    var bfseq = Seq.empty[RegField]
    
    bitfields.foreach {bf =>
      bf match {
        case WavRW(c, r, n, d, corescan, iddq, highz, bscan, bflop) => {
          val rf = WavRWReg(c, r, n, d, regName, regDesc, corescan, iddq, highz, bscan, bflop)
          bfseq = bfseq :+ rf
        }
        case WavW1C(i, o, n, d) => {
          val rf = WavW1CReg(i, o, n, d, regName, regDesc)
          bfseq = bfseq :+ rf
        }
        case WavRO(c, n, d, demet, bflop) => {
          val rf = WavROReg(c, n, d, demet, regName, regDesc, bflop)
          bfseq = bfseq :+ rf
        }
        case WavRSVD(w) => {
          val rf = WavRSVDReg(w, regName, regDesc)
          bfseq = bfseq :+ rf
        }
        case WavRWMux(i, m, rr, mr, n, d, corescan, iddq, highz, bscan, bflop) => {
          val rf =  WavRWMuxReg(i, m, rr, mr, n, d, regName, regDesc, corescan, iddq, highz, bscan, bflop)
          bfseq = bfseq ++ rf
        }
        
        case WavROBundle(b, n, d, demet) => {
          val rf = WavROBundleReg(b, n, d, demet, regName, regDesc)
          bfseq = bfseq ++ rf
        }
        
        case WavSWBundle(b, n, d, demet) => {
          val rf = WavSWBundleReg(b, n, d, demet, regName, regDesc)
          bfseq = bfseq ++ rf
        }
        
      }
    }
    
    (regAddr, bfseq)
  }
}

