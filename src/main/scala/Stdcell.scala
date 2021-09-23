package wavd2d

import chisel3._
import chisel3.util._
import chisel3.util.HasBlackBoxInline
import freechips.rocketchip.config._

/**
  *   Wavious Stdcell Wrappers
  *
  */

/** 
  * A field available in Parameters to determine if we use behavioral for certain Stdcell wrappers
  * This is so the wav_stdcell_lib.sv doesn't have to be included for simulation. 
  */
case object WavStdcellBehavioral extends Field[Boolean](true)


/**
  *
  */
class wav_demet_r (width : Int = 1) extends BlackBox(Map("DWIDTH" -> width)) {
  val io = IO(new Bundle{
    val i_clk   = Input (Clock())
    val i_rst   = Input (Reset())
    val i_d     = Input (UInt(width.W))
    val o_q     = Output(UInt(width.W))
  })
}

class WavDemetReset [T <: Data](gen : T)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in  = Input (gen.cloneType)
    val out = Output(gen.cloneType)
  })
  
  if(p(WavStdcellBehavioral)) {
    val f1 = RegInit(0.U(gen.getWidth.W))
    val f2 = RegInit(0.U(gen.getWidth.W))
    f1      := io.in
    f2      := f1
    io.out  := f2
  } else {
    val wdr = Module(new wav_demet_r(gen.getWidth))
  
    wdr.io.i_clk := clock
    wdr.io.i_rst := reset
    wdr.io.i_d   := io.in
    io.out       := wdr.io.o_q
  }
  
}


object WavDemetReset{
  def apply[T <: Data](in : T)(implicit p: Parameters): T = {
    val demet = Module(new WavDemetReset(in)(p))
    demet.io.in := in
    demet.io.out
  }
}

/**
  *
  */
class wav_demet_s (width : Int = 1) extends BlackBox(Map("DWIDTH" -> width)) {
  val io = IO(new Bundle{
    val i_clk   = Input (Clock())
    val i_set   = Input (Reset())
    val i_d     = Input (UInt(width.W))
    val o_q     = Output(UInt(width.W))
  })
}

class WavDemetSet [T <: Data](gen : T)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in  = Input (gen.cloneType)
    val out = Output(gen.cloneType)
  })
  
  if(p(WavStdcellBehavioral)) {
    val f1 = RegInit(1.U(gen.getWidth.W))   //make all ones
    val f2 = RegInit(1.U(gen.getWidth.W))
    f1      := io.in
    f2      := f1
    io.out  := f2
  } else {
    val wds = Module(new wav_demet_s(gen.getWidth))
  
    wds.io.i_clk := clock
    wds.io.i_set := reset   //note, we still use the implicit "reset" here
    wds.io.i_d   := io.in
    io.out       := wds.io.o_q
  }
  
}


object WavDemetSet{
  def apply[T <: Data](in : T)(implicit p: Parameters): T = {
    val demet = Module(new WavDemetSet(in)(p))
    demet.io.in := in
    demet.io.out
  }
}


/**
  *
  */
class wav_mux (width : Int = 1) extends BlackBox(Map("DWIDTH" -> width)) {
  val io = IO(new Bundle{
    val i_sel   = Input (Bool())
    val i_a     = Input (UInt(width.W))
    val i_b     = Input (UInt(width.W))
    val o_z     = Output(UInt(width.W))
  })
}

class WavClockMux [T <: Data](a : T, b : T)(implicit p: Parameters) extends Module{
  //Compute the larger of the width and set the parameter
  val outType = if(a.getWidth > b.getWidth) a.cloneType else b.cloneType
  
  val io = IO(new Bundle{
    val i_sel   = Input (Bool())
    val i_a     = Input (a.cloneType)
    val i_b     = Input (b.cloneType)
    val o_z     = Output(outType.cloneType)
  })
  
  if(p(WavStdcellBehavioral)) {
    io.o_z := Mux(io.i_sel, io.i_b, io.i_a)
  } else {
    val wmux = Module(new wav_mux(outType.getWidth))

    wmux.io.i_sel   := io.i_sel
    wmux.io.i_a     := io.i_a
    wmux.io.i_b     := io.i_b
    io.o_z          := wmux.io.o_z
  }
  
}


//Be sure to note that this follows the traditional Mux setting
object WavClockMux{
  def apply[T <: Data](sel : Bool, b : T, a : T)(implicit p: Parameters): T = {
    val mux = Module(new WavClockMux(a, b))
    mux.io.i_sel    := sel
    mux.io.i_a      := a
    mux.io.i_b      := b
    mux.io.o_z
  }
}


/**
  *
  */
class wav_and (width : Int = 1) extends BlackBox(Map("DWIDTH" -> width)) {
  val io = IO(new Bundle{
    val i_a     = Input (UInt(width.W))
    val i_b     = Input (UInt(width.W))
    val o_z     = Output(UInt(width.W))
  })
}

class WavAnd [T <: Data](a : T, b : T)(implicit p: Parameters) extends Module{
  //Compute the larger of the width and set the parameter
  val outType = if(a.getWidth > b.getWidth) a.cloneType else b.cloneType
  
  val io = IO(new Bundle{
    val i_a = Input (a.cloneType)
    val i_b = Input (b.cloneType)
    val o_z = Output(outType.cloneType)
  })
  
  if(p(WavStdcellBehavioral)) {
    io.o_z := io.i_a.asUInt & io.i_b.asUInt
  } else {
    val wand = Module(new wav_and(outType.getWidth))
  
    wand.io.i_a := io.i_a
    wand.io.i_b := io.i_b
    io.o_z      := wand.io.o_z
  }
  
}


object WavAnd{
  def apply[T <: Data](a : T, b : T)(implicit p: Parameters): T = {
    val wand = Module(new WavAnd(a, b)(p))
    wand.io.i_a := a
    wand.io.i_b := b
    wand.io.o_z
  }
}

/**
  *
  */
class wav_or(width : Int = 1) extends BlackBox(Map("DWIDTH" -> width)) {
  val io = IO(new Bundle{
    val i_a     = Input (UInt(width.W))
    val i_b     = Input (UInt(width.W))
    val o_z     = Output(UInt(width.W))
  })
}


class WavOr [T <: Data](a : T, b : T)(implicit p: Parameters) extends Module{
  //Compute the larger of the width and set the parameter
  val outType = if(a.getWidth > b.getWidth) a.cloneType else b.cloneType
  
  val io = IO(new Bundle{
    val i_a = Input (a.cloneType)
    val i_b = Input (b.cloneType)
    val o_z = Output(outType.cloneType)
  })
  
  if(p(WavStdcellBehavioral)) {
    io.o_z := io.i_a.asUInt | io.i_b.asUInt
  } else {
    val wor = Module(new wav_or(outType.getWidth))
  
    wor.io.i_a  := io.i_a
    wor.io.i_b  := io.i_b
    io.o_z      := wor.io.o_z
  }
  
}


object WavOr{
  def apply[T <: Data](a : T, b : T)(implicit p: Parameters): T = {
    val wor = Module(new WavOr(a, b)(p))
    wor.io.i_a := a
    wor.io.i_b := b
    wor.io.o_z
  }
}


/**
  *
  */
class wav_inv (width : Int = 1) extends BlackBox(Map("DWIDTH" -> width)) {
  val io = IO(new Bundle{
    val i_a     = Input (UInt(width.W))
    val o_z     = Output(UInt(width.W))
  })
}

class WavClockInv [T <: Data](gen : T)(implicit p: Parameters) extends Module{  
  val io = IO(new Bundle{
    val a   = Input (gen.cloneType)
    val z   = Output(gen.cloneType)
  })
  
  if(p(WavStdcellBehavioral)) {
    io.z := ~io.a.asUInt 
  } else {
    val winv = Module(new wav_inv(gen.getWidth))
  
    winv.io.i_a := io.a
    io.z        := winv.io.o_z
  }
  
}


object WavClockInv{
  def apply[T <: Data](in : T)(implicit p: Parameters): T = {
    val winv = Module(new WavClockInv(in)(p))
    winv.io.a := in
    winv.io.z
  }
}


/**
  *
  */
class wav_reset_sync extends BlackBox{
  val io = IO(new Bundle{
    val clk       = Input (Bool())
    val scan_ctrl = Input (Bool())
    val reset_in  = Input (Bool())
    val reset_out = Output(Bool())
  })
}

class WavResetSync ()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle{
    val clk       = Input (Bool())
    val scan_ctrl = Input (Bool())
    val reset_in  = Input (Bool())
    val reset_out = Output(Bool())
  })
  
  if(p(WavStdcellBehavioral)) {
    val scan_low        = WavClockInv(io.scan_ctrl)
    val reset_scan_gate = WavAnd(scan_low, io.reset_in)
    val reset_sync      = Wire(Bool())
    val clkdomain = withClockAndReset(io.clk.asClock, reset_scan_gate.asAsyncReset) {
      reset_sync := WavDemetSet(0.U)
      //io.reset_out := WavAnd(scan_low, reset_sync)
    }
    io.reset_out := WavAnd(scan_low, reset_sync)
  } else {
    val reset_sync = Module(new wav_reset_sync)
    reset_sync.io.clk       := io.clk
    reset_sync.io.scan_ctrl := io.scan_ctrl
    reset_sync.io.reset_in  := io.reset_in
    io.reset_out            := reset_sync.io.reset_out
    
  }
}

object WavResetSync{
  def apply[T <: Data](clk : Bool, reset_in: Bool, scan_ctrl: Bool)(implicit p: Parameters): Bool = {
    val wrs = Module(new WavResetSync()(p))
    wrs.io.clk        := clk
    wrs.io.reset_in   := reset_in
    wrs.io.scan_ctrl  := scan_ctrl
    wrs.io.reset_out    
  }
}




/**
  *
  */
class wav_clk_mux_gf extends BlackBox {
  val io = IO(new Bundle{
    val i_clk_0       = Input (Bool())   
    val i_clk_1       = Input (Bool())   
    val i_clk_rst_0   = Input (Bool())   
    val i_clk_rst_1   = Input (Bool())   
    val i_sel         = Input (Bool())   
    val i_test_en     = Input (Bool())   
    val i_cgc_en      = Input (Bool())   
    val o_sel_0       = Output(Bool())   
    val o_sel_1       = Output(Bool())   
    val o_clk         = Output(Bool())
  })
}

class WavClkMuxGF (implicit p: Parameters) extends RawModule{  
  val io = IO(new Bundle{
    val clk_0       = Input (Bool())   
    val clk_1       = Input (Bool())   
    val clk_rst_0   = Input (Bool())   
    val clk_rst_1   = Input (Bool())   
    val sel         = Input (Bool())   
    val test_en     = Input (Bool())   
    val cgc_en      = Input (Bool())   
    //val sel_0       = Output(Bool())   
    //val sel_1       = Output(Bool())   
    val clk         = Output(Bool())
  })
  

    val wav_clk_mux_gf = Module(new wav_clk_mux_gf)
    wav_clk_mux_gf.io.i_clk_0     <> io.clk_0    
    wav_clk_mux_gf.io.i_clk_1     <> io.clk_1    
    wav_clk_mux_gf.io.i_clk_rst_0 <> io.clk_rst_0
    wav_clk_mux_gf.io.i_clk_rst_1 <> io.clk_rst_1
    wav_clk_mux_gf.io.i_sel       <> io.sel      
    wav_clk_mux_gf.io.i_test_en   <> io.test_en  
    wav_clk_mux_gf.io.i_cgc_en    <> io.cgc_en   
    //wav_clk_mux_gf.io.o_sel_0     <> io.sel_0    
    //wav_clk_mux_gf.io.o_sel_1     <> io.sel_1    
    wav_clk_mux_gf.io.o_clk       <> io.clk      
  
}


class wav_latch_model extends BlackBox with HasBlackBoxInline{
  val io = IO(new Bundle{
    val i_clk = Input (Bool())
    val i_d   = Input (Bool())
    val o_q   = Output(Bool())
  })
  
  setInline("wav_latch_model.sv",
    """module wav_latch_model(
     |   input  logic i_clk,
     |   input  logic i_d,
     |   output logic o_q
     |  );
     |  always_latch
     |    if(i_clk)
     |      o_q <= i_d;
     | endmodule
    """.stripMargin)
}

/**
  *
  */
class wav_clock_gate extends BlackBox{
  val io = IO(new Bundle{
    val clk_in          = Input (Bool())
    val en              = Input (Bool())
    val test_en         = Input (Bool())
    val clk_out         = Output(Bool())
  })
}

class WavClockGate (val withDemet: Boolean = true)(implicit p: Parameters) extends Module{  
  val io = IO(new Bundle{
    //We want to define the explicit clock here
    val clk_in    = Input (Bool())
    val reset_in  = Input (Bool())
    val enable    = Input (Bool())
    val test_en   = Input (Bool())
    val clk_out   = Output(Bool())
  })
  
  if(p(WavStdcellBehavioral)) {
    withClockAndReset(io.clk_in.asClock, io.reset_in.asAsyncReset){
      val clk_en  = Wire(Bool())
      if(withDemet){
        clk_en  := (WavDemetReset(io.enable) | io.test_en)
      } else {
        clk_en := (io.enable | io.test_en)
      }
      
      val clk_inv = WavClockInv(io.clk_in)
      val latch   = Module(new wav_latch_model)
        
      latch.io.i_clk      := clk_inv
      latch.io.i_d        := clk_en
      val clk_en_latched  = latch.io.o_q
      io.clk_out  := WavAnd(clk_en_latched, io.clk_in)
      
    }
  } else {
    val wav_cg = Module(new wav_clock_gate)
    wav_cg.io.clk_in  := io.clk_in
    if(withDemet) {
      wav_cg.io.en    := withClockAndReset(io.clk_in.asClock, io.reset_in.asAsyncReset){WavDemetReset(io.enable)}
    } else {
      wav_cg.io.en    := io.enable
    }
    wav_cg.io.test_en := io.test_en
    io.clk_out        := wav_cg.io.clk_out
    
  }
  
}

object WavClockGate{
  def apply(clk_in : Bool, reset_in : Bool, enable : Bool, test_en : Bool)(implicit p: Parameters): Bool = {
    val wcg = Module(new WavClockGate(true)(p))
    wcg.io.clk_in   := clk_in
    wcg.io.reset_in := reset_in
    wcg.io.enable   := enable
    wcg.io.test_en  := test_en
    wcg.io.clk_out
  }
  
  def apply(clk_in : Bool, reset_in : Bool, enable : Bool, test_en : Bool, withDemet : Boolean = true)(implicit p: Parameters): Bool = {
    val wcg = Module(new WavClockGate(withDemet)(p))
    wcg.io.clk_in   := clk_in
    wcg.io.reset_in := reset_in
    wcg.io.enable   := enable
    wcg.io.test_en  := test_en
    wcg.io.clk_out
  }
}

// 
// 
// object WavClockInv{
//   def apply[T <: Data](in : T)(implicit p: Parameters): T = {
//     val winv = Module(new WavClockInv(in)(p))
//     winv.io.a := in
//     winv.io.z
//   }
// }
