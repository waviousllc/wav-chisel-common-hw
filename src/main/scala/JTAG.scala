package wavd2d


import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.ChiselStage


import freechips.rocketchip.amba._
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

/**
  *   Direction is with respect to an incoming TAP
  */
class WavJtagBundle extends Bundle{
  val tck         = Input (Bool())
  val trst_n      = Input (Bool())
  val tms         = Input (Bool())
  val tdi         = Input (Bool())
  val tdo         = Output(Bool())
}

/**
  *   See IEEE-1838 for more details
  *   Each STAP receives a normal TAP interface and 3 additional signals
  *   The TDI/TDO in this case would be the "sk" and "sN" you see in the 
  *   spec. 
  */
class WavJtagStapBundle(val numStaps: Int = 1) extends Bundle{
  val tap         = new WavJtagBundle
  val enable      = Input (Bool())
  val select      = Input (UInt(numStaps.W))
  val rti_or_tlr  = Input (UInt(numStaps.W))
  
  // Can we put in a connection method that takes the "previous"
  // STAP port and connects up the TDO of that STAP to the TDI of this one?
}


class WavBSRBundle extends Bundle{
  val tck     = Input (Bool())
  val trst_n  = Input (Bool())
  val bsr_mode= Input (Bool())
  val capture = Input (Bool())
  val shift   = Input (Bool())
  val update  = Input (Bool())
  val tdi     = Input (Bool())
  val tdo     = Output(Bool())
  
  //Used to chain multiple BSRBundles together
  def -> (that: WavBSRBundle): WavBSRBundle = {
    that.tck      := this.tck
    that.trst_n   := this.trst_n
    that.bsr_mode := this.bsr_mode
    that.capture  := this.capture
    that.shift    := this.shift
    that.update   := this.update
    that.tdi      := this.tdo
    that
  }
  
  //Connect this bundle to another BSR
  def -> (that: WavBSR): WavBSR = {
    that.io.jtag.tdi := this.tdo
    that
  }
  
  //Generally used to connect to an end TDO
  def -> (that: Bool): Bool = {
    that := this.tdo
    that
  }
  
}

class wav_jtag_reg(
  val width   : Int = 1,
  val noByp   : Int = 1,
  val resetVal: Int = 0,
  val bsrMask : Int = 0
) extends BlackBox(Map(
  "DWIDTH"    -> width,
  "NO_BYP"    -> noByp,
  "RESET_VAL" -> resetVal,
  "BSR_MASK"  -> bsrMask
)){
  val io = IO(new Bundle{
    val i_tck       = Input (Bool())
    val i_trst_n    = Input (Bool())
    val i_bsr_mode  = Input (Bool())
    val i_capture   = Input (Bool())
    val i_shift     = Input (Bool())
    val i_update    = Input (Bool())
    val i_pi        = Input (UInt(width.W))
    val o_po        = Output(UInt(width.W))
    val i_tdi       = Input (Bool())
    val o_tdo       = Output(Bool())
  })
}


/**
  *   Boundary Scan Register
  *   Can be placed and wired up manually or can be done through the method overload
  *   
  *
  *   the 'tdo' method offers a less verbose way to get the TDO of this cell
  *
  *   WANT TO REPLACE THIS WITH DIPLOMACY, HOWEVER THERE ARE SOME DIFFICULT ISSUES TO SOLVE
  */
class WavBSR(
  val width   : Int = 1,
  val noByp   : Int = 1,
  val resetVal: Int = 0
) extends Module{
  val io = IO(new Bundle{
    val jtag    = new WavBSRBundle
    val pi      = Input (UInt(width.W))
    val po      = Output(UInt(width.W))
  })
  
  val jtag_reg = Module(new wav_jtag_reg(width=width, noByp=noByp, resetVal=resetVal))
  jtag_reg.io.i_tck     := io.jtag.tck
  jtag_reg.io.i_trst_n  := io.jtag.trst_n
  jtag_reg.io.i_bsr_mode:= io.jtag.bsr_mode
  jtag_reg.io.i_capture := io.jtag.capture
  jtag_reg.io.i_shift   := io.jtag.shift
  jtag_reg.io.i_update  := io.jtag.update
  
  jtag_reg.io.i_tdi     := io.jtag.tdi
  
  jtag_reg.io.i_pi      := io.pi
  io.po                 := jtag_reg.io.o_po
  
  io.jtag.tdo           := jtag_reg.io.o_tdo
  

  
  def -> (that: WavBSR): WavBSR = {
    that.io.jtag.tdi := this.tdo
    that
  }
  
  def -> (that: Bool): Bool = {
    that := this.tdo
    that
  }
  
  def tdo = io.jtag.tdo
}


object WavBSR{
  def apply[T <: Data](in : T, out : T, jtag: WavBSRBundle): WavBSR = {
    require(in.getWidth == out.getWidth, "In/out width for WavBSR is not the same!")
    
    val bsr = Module(new WavBSR(width=in.getWidth, noByp=1))
    bsr.io.jtag <> jtag
    bsr.io.pi   := in
    out         := bsr.io.po
    bsr
  }
  
  /** 
    *   No in/out given on creation. Usually used for SW 
    *   Can always be overridden later
    */
  def apply[T <: Data](width: Int, jtag: WavBSRBundle): WavBSR = {
    val bsr = Module(new WavBSR(width=width, noByp=1))
    bsr.io.jtag <> jtag
    bsr.io.pi   := false.B
    bsr
  }
}




/**********************************************************************
*   
**********************************************************************/
class wav_jtag_top(
  val apbAddrWidth  : Int     = 64,
  val idCodeReset   : BigInt  = 0x77617601,
  val includeApb    : Int     = 0,
  val numStaps      : Int     = 1
)extends BlackBox(Map(
  "APB_ADDR_WIDTH"  -> apbAddrWidth,
  "IDCODE_RESET"    -> idCodeReset,
  "INCLUDE_APB"     -> includeApb,
  "NUM_STAPS"       -> numStaps
)){
  val io = IO(new Bundle{
    val i_tck               = Input (Bool())
    val i_trst_n            = Input (Bool())
    val i_tms               = Input (Bool())
    val i_tdi               = Input (Bool())
    val o_tdo               = Output(Bool())  
       
    val o_dsr_mode          = Output(Bool())
    val o_dsr_tdo           = Output(Bool())
    val i_dsr_tdo           = Input (Bool())
    val o_dsr_shift         = Output(Bool())
    val o_dsr_capture       = Output(Bool())
    val o_dsr_update        = Output(Bool())
    
    val o_dsr_dft_mode      = Output(Bool())
    val o_dsr_dft_tdo       = Output(Bool())
    val i_dsr_dft_tdo       = Input (Bool())
    val o_dsr_dft_shift     = Output(Bool())
    val o_dsr_dft_capture   = Output(Bool())
    val o_dsr_dft_update    = Output(Bool())
    
    val o_bsr_mode          = Output(Bool())
    val o_bsr_tdo           = Output(Bool())
    val i_bsr_tdo           = Input (Bool())
    val o_bsr_shift         = Output(Bool())
    val o_bsr_capture       = Output(Bool())
    val o_bsr_update        = Output(Bool())
    
    val o_bist_ctrl_mode    = Output(Bool())
    val o_bist_ctrl_tdo     = Output(Bool())
    val i_bist_ctrl_tdo     = Input (Bool())
    val o_bist_ctrl_shift   = Output(Bool())
    val o_bist_ctrl_capture = Output(Bool())
    val o_bist_ctrl_update  = Output(Bool())
    
    val o_stap_enable       = Output(Bool())
    val o_stap_sel          = Output(UInt(numStaps.W))     
    val o_stap_rti_or_tlr   = Output(UInt(numStaps.W))
    val o_stap_tdi_sk       = Output(Bool())
    val i_stap_tdo_s1       = Input (Bool())
    
    val i_freeze_n          = Input (Bool())
    val i_highz_n           = Input (Bool())
    val i_iddq_mode         = Input (Bool())
    val o_freeze_n          = Output(Bool())
    val o_highz_n           = Output(Bool())
    val o_iddq_mode         = Output(Bool())
    
    val o_scan_freq_en      = Output(UInt(8.W))
    val o_scan_cgc_ctrl     = Output(Bool())
    val o_scan_rst_ctrl     = Output(Bool())
    val o_scan_set_ctrl     = Output(Bool())
    val o_scan_tdf_mode     = Output(Bool())
    
    val i_apb_clk           = Input (Bool())
    val i_apb_reset         = Input (Bool())
    val o_apb_psel          = Output(Bool())
    val o_apb_penable       = Output(Bool())
    val o_apb_pwrite        = Output(Bool())
    val o_apb_pwdata        = Output(UInt(32.W))
    val o_apb_paddr         = Output(UInt(apbAddrWidth.W))     
    val i_apb_pslverr       = Input (Bool())
    val i_apb_pready        = Input (Bool())
    val i_apb_prdata        = Input (UInt(32.W))
  })
}

class WavJtag(
  val apbAddrWidth  : Int     = 64,
  val idCodeReset   : BigInt  = 0x77617601,
  val includeApb    : Int     = 0,
  val numStaps      : Int     = 1
)(implicit p: Parameters) extends LazyModule{
  
  val node = APBMasterNode(
    portParams = Seq(APBMasterPortParameters(masters = Seq(APBMasterParameters(name = "jtag2apb"))))
  )
  
  override lazy val module = new LazyModuleImp(this) with RequireAsyncReset{
    val io = IO(new Bundle{
      val tap               = new WavJtagBundle             //From TAP Ports
      val dsr               = Flipped(new WavBSRBundle)     //Data Shift Register
      val dsr_dft           = Flipped(new WavBSRBundle)     //DFT control
      val bsr               = Flipped(new WavBSRBundle)     //Boundary Scan Shift Register
      val bist_ctrl         = Flipped(new WavBSRBundle)     //BIST/MBIST
      val stap              = Flipped(new WavJtagStapBundle(numStaps))
      
      //Chosing not to have the scan signals be a part of the normal ScanBundle (for now) since use cases may vary
      val i_freeze_n        = Input (Bool())
      val i_highz_n         = Input (Bool())
      val i_iddq_mode       = Input (Bool())
      val o_freeze_n        = Output(Bool())
      val o_highz_n         = Output(Bool())
      val o_iddq_mode       = Output(Bool())

      val o_scan_freq_en    = Output(UInt(8.W))
      val o_scan_cgc_ctrl   = Output(Bool())
      val o_scan_rst_ctrl   = Output(Bool())
      val o_scan_set_ctrl   = Output(Bool())
      val o_scan_tdf_mode   = Output(Bool())
    })
    
    val jtag = Module(new wav_jtag_top(apbAddrWidth   = apbAddrWidth,
                                       idCodeReset    = idCodeReset,
                                       includeApb     = includeApb,
                                       numStaps       = numStaps))
    
    val apb_port  = node.out.head._1
    jtag.io.i_apb_clk       := clock.asBool
    jtag.io.i_apb_reset     := reset.asBool
    
    apb_port.psel           := jtag.io.o_apb_psel
    apb_port.penable        := jtag.io.o_apb_penable
    apb_port.pwrite         := jtag.io.o_apb_pwrite
    apb_port.pwdata         := jtag.io.o_apb_pwdata
    apb_port.paddr          := jtag.io.o_apb_paddr
    apb_port.pprot          := 0.U
    apb_port.pstrb          := "hf".U
    jtag.io.i_apb_pslverr   := apb_port.pslverr
    jtag.io.i_apb_pready    := apb_port.pready
    jtag.io.i_apb_prdata    := apb_port.prdata
    
    jtag.io.i_tck           := io.tap.tck
    jtag.io.i_trst_n        := io.tap.trst_n
    jtag.io.i_tms           := io.tap.tms
    jtag.io.i_tdi           := io.tap.tdi
    io.tap.tdo              := jtag.io.o_tdo
    
    
    io.dsr.tck              := io.tap.tck
    io.dsr.trst_n           := io.tap.trst_n
    io.dsr.bsr_mode         := jtag.io.o_dsr_mode
    io.dsr.capture          := jtag.io.o_dsr_capture
    io.dsr.shift            := jtag.io.o_dsr_shift
    io.dsr.update           := jtag.io.o_dsr_update
    io.dsr.tdi              := jtag.io.o_dsr_tdo        //Going out of the TAP INTO the TDI of the IP
    jtag.io.i_dsr_tdo       := io.dsr.tdo               //Coming INTO the TAP FROM the TDO of the IP
    
    io.dsr_dft.tck          := io.tap.tck
    io.dsr_dft.trst_n       := io.tap.trst_n
    io.dsr_dft.bsr_mode     := jtag.io.o_dsr_dft_mode
    io.dsr_dft.capture      := jtag.io.o_dsr_dft_capture
    io.dsr_dft.shift        := jtag.io.o_dsr_dft_shift
    io.dsr_dft.update       := jtag.io.o_dsr_dft_update
    io.dsr_dft.tdi          := jtag.io.o_dsr_dft_tdo       
    jtag.io.i_dsr_dft_tdo   := io.dsr_dft.tdo              
    
    io.bsr.tck              := io.tap.tck
    io.bsr.trst_n           := io.tap.trst_n
    io.bsr.bsr_mode         := jtag.io.o_bsr_mode
    io.bsr.capture          := jtag.io.o_bsr_capture
    io.bsr.shift            := jtag.io.o_bsr_shift
    io.bsr.update           := jtag.io.o_bsr_update
    io.bsr.tdi              := jtag.io.o_bsr_tdo        
    jtag.io.i_bsr_tdo       := io.bsr.tdo     
    
    io.bist_ctrl.tck        := io.tap.tck
    io.bist_ctrl.trst_n     := io.tap.trst_n
    io.bist_ctrl.bsr_mode   := jtag.io.o_bist_ctrl_mode
    io.bist_ctrl.capture    := jtag.io.o_bist_ctrl_capture
    io.bist_ctrl.shift      := jtag.io.o_bist_ctrl_shift
    io.bist_ctrl.update     := jtag.io.o_bist_ctrl_update
    io.bist_ctrl.tdi        := jtag.io.o_bist_ctrl_tdo       
    jtag.io.i_bist_ctrl_tdo := io.bist_ctrl.tdo              
    
    io.stap.tap.tck         := io.tap.tck
    io.stap.tap.trst_n      := io.tap.trst_n
    io.stap.tap.tms         := io.tap.tms
    
    io.stap.tap.tdi         := jtag.io.o_stap_tdi_sk      //This is coming OUT of the TAP going INTO the TDI of IP
    jtag.io.i_stap_tdo_s1   := io.stap.tap.tdo            //Check out the IEEE 1838 spec for some more details
    
    io.stap.enable          := jtag.io.o_stap_enable
    io.stap.select          := jtag.io.o_stap_sel
    io.stap.rti_or_tlr      := jtag.io.o_stap_rti_or_tlr
    
    jtag.io.i_freeze_n      := io.i_freeze_n
    jtag.io.i_highz_n       := io.i_highz_n
    jtag.io.i_iddq_mode     := io.i_iddq_mode
    
    io.o_freeze_n           := jtag.io.o_freeze_n 
    io.o_highz_n            := jtag.io.o_highz_n  
    io.o_iddq_mode          := jtag.io.o_iddq_mode
    
    io.o_scan_freq_en       := jtag.io.o_scan_freq_en 
    io.o_scan_cgc_ctrl      := jtag.io.o_scan_cgc_ctrl
    io.o_scan_rst_ctrl      := jtag.io.o_scan_rst_ctrl
    io.o_scan_set_ctrl      := jtag.io.o_scan_set_ctrl
    io.o_scan_tdf_mode      := jtag.io.o_scan_tdf_mode
    
  }
}



class wav_jtag_stap extends BlackBox{
  val io = IO(new Bundle{
    val i_tck           = Input (Bool())
    val i_tms           = Input (Bool())
    val i_trst_n        = Input (Bool())
    val i_tdi           = Input (Bool())
    val o_tdo           = Output(Bool())
    val i_enable        = Input (Bool())
    val i_select        = Input (Bool())
    val i_rti_or_tlr    = Input (Bool())
    val o_exdie_tck     = Output(Bool())
    val o_exdie_tms     = Output(Bool())
    val o_exdie_trst_n  = Output(Bool())
    val o_exdie_tdi     = Output(Bool())
    val i_exdie_tdo     = Input (Bool())
  })
}

class WavJtagStap extends Module{
  val io = IO(new Bundle{
    val stapIf  = new WavJtagStapBundle(1)
    val extap   = Flipped(new WavJtagBundle)
  })
  
  val stap = Module(new wav_jtag_stap)
  stap.io.i_tck         := io.stapIf.tap.tck
  stap.io.i_tms         := io.stapIf.tap.tms
  stap.io.i_trst_n      := io.stapIf.tap.trst_n
  stap.io.i_tdi         := io.stapIf.tap.tdi
  io.stapIf.tap.tdo     := stap.io.o_tdo         
  stap.io.i_enable      := io.stapIf.enable
  stap.io.i_select      := io.stapIf.select(0)
  stap.io.i_rti_or_tlr  := io.stapIf.rti_or_tlr(0)
  
  io.extap.tck          := stap.io.o_exdie_tck   
  io.extap.tms          := stap.io.o_exdie_tms   
  io.extap.trst_n       := stap.io.o_exdie_trst_n
  io.extap.tdi          := stap.io.o_exdie_tdi   
  stap.io.i_exdie_tdo   := io.extap.tdo
}

/**********************************************************************
*   Generation Wrappers
**********************************************************************/

// //You can use this to see how it's generated
// class JTAGWrapper extends Module{
//   val io = IO(new Bundle{
//     val jtag    = new WavBSRBundle
//     val in      = Input (UInt(4.W))
//     val in2     = Input (UInt(2.W))
//     val in3     = Input (Bool())
//     val out     = Output(UInt(4.W))
//     val out2    = Output(UInt(2.W))
//     val out3    = Output(Bool())
//   })
//   
//   val bsr2 = WavBSR(io.in2, io.out2, io.jtag, "end") := WavBSR(io.in,  io.out,  io.jtag) := WavBSR(io.in3,  io.out3,  io.jtag)
//   
//   io.jtag.tdo := bsr2.tdo
//   
// }
// 
// 
// object JTAGWrapperGen extends App {  
//   
//   val wlinkverilog = (new ChiselStage).emitVerilog(
//     new JTAGWrapper,
//      
//     //args
//     Array("--target-dir", "output/", "--no-dce")
//   )
//   
// }
