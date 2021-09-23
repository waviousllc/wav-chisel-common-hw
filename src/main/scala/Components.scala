package wavd2d

import chisel3._
import chisel3.util._
import chisel3.util.HasBlackBoxInline
import freechips.rocketchip.config._

import freechips.rocketchip.config.{Parameters, Field, Config}



case object WavComponentPrefix extends Field[Option[String]](None)


/**
  *   Translates a pulse between clock domains.
  *   Pulses are not expected to be followed cycle after cycle, so proper timing
  *   should be ensured in the design
  *
  */
class WavSyncPulse()(implicit p: Parameters) extends RawModule{
  val io = IO(new Bundle{
    val clk_in        = Input (Clock())
    val clk_in_reset  = Input (AsyncReset())
    val data_in       = Input (Bool())
    
    val clk_out       = Input (Clock())
    val clk_out_reset = Input (AsyncReset())
    val data_out      = Output(Bool())
  })
  
  
  val clk_in_pulse = WireInit(false.B)
  withClockAndReset(io.clk_in, io.clk_in_reset){
    val clk_in_pulse_reg = RegNext(next=(Mux(io.data_in, ~clk_in_pulse, clk_in_pulse)), init=false.B)
    clk_in_pulse  := clk_in_pulse_reg
  }
  
  withClockAndReset(io.clk_out, io.clk_out_reset){
    val pulse_demeted     = WavDemetReset(clk_in_pulse)
    val pulse_demeted_ff3 = RegNext(next=pulse_demeted, init=false.B)
    io.data_out  := pulse_demeted ^ pulse_demeted_ff3
  }
  
}




class wav_pi_control_encode()(implicit p: Parameters) extends BlackBox with HasBlackBoxInline{
  val io = IO(new Bundle{
    val clk       = Input (Bool())
    val reset     = Input (Bool())
    val oneup     = Input (Bool())
    val onedown   = Input (Bool())
    val pi_bin    = Output(UInt(6.W))
    val pi_ctrl   = Output(UInt(16.W))
    val pi_quad   = Output(UInt(2.W))
  })
  
  var prefix = if(p(WavComponentPrefix) != None) p(WavComponentPrefix)+"_" else ""
  override val desiredName = s"${prefix}wav_pi_control_encode" 
  
  setInline(s"${desiredName}.v",
    s"""module ${desiredName}(
      |   input  wire         clk,
      |   input  wire         reset,
      |   input  wire         oneup,
      |   input  wire         onedown,
      |   output reg  [5:0]   pi_bin,
      |   output reg  [15:0]  pi_ctrl,
      |   output reg  [1:0]   pi_quad
      |  );
      |
      |  reg  [1:0]   pi_quad_nxtup;
      |  reg  [1:0]   pi_quad_nxtdown;
      |  wire         quaddown;
      |  wire         quadup;
      |  wire [1:0]   pi_quad_in;
      |  wire [15:0]  pi_ctrl_in;
      |  wire [3:0]   pi_bin_low_in;
      |  wire [1:0]   pi_bin_high_in;
      |
      |
      |  assign pi_ctrl_in  = oneup   ? (pi_quad[1]^pi_quad[0]) ? {1'b1, pi_ctrl[15:1]} : {1'b0, pi_ctrl[15:1]} :
      |                       onedown ? {pi_ctrl[14:0], ~pi_ctrl[15]} : pi_ctrl;
      |
      |  assign quaddown    = onedown & ((pi_ctrl == 16'h0000) || (pi_ctrl == 16'hffff));
      |  assign quadup      = oneup   & ((pi_ctrl == 16'hFFFE) || (pi_ctrl == 16'h0001));
      |  assign pi_quad_in  = quaddown ? pi_quad_nxtdown : (quadup ? pi_quad_nxtup : pi_quad);
      |
      |  //assign pi_bin_in    = oneup ? pi_bin + 1'b1 ? onedown ? pi_bin - 1'b1 : pi_bin;
      |  assign pi_bin_low_in  = oneup ? pi_bin[3:0] + 1'b1 : onedown ? pi_bin[3:0] - 1'b1 : pi_bin[3:0];
      |  //assign pi_quad_in   = (pi_bin_in[5:4] == 2'b00) ? 2'b01 : (pi_bin_in[5:4] == 2'b01) ? 2'b00 : pi_bin_in[5:4];
      |  assign pi_bin_high_in = (pi_quad_in == 2'b01) ? 2'b00 : (pi_quad_in == 2'b00) ? 2'b01 : pi_quad_in;
      |
      |always @(posedge clk or posedge reset) begin
      |  if (reset == 1'b1) begin
      |    pi_bin  <= 6'b000000;
      |    pi_ctrl <= 16'h0000;
      |    pi_quad <= 2'b01;
      |  end else begin
      |    pi_bin  <= {pi_bin_high_in, pi_bin_low_in};
      |    pi_ctrl <= pi_ctrl_in;
      |    pi_quad <= pi_quad_in;
      |  end
      |end
      |
      |// pi_quad is gray coded
      |
      |always @(*) begin
      |  case (pi_quad)
      |    2'b00 : pi_quad_nxtdown = 2'b01;
      |    2'b01 : pi_quad_nxtdown = 2'b11;
      |    2'b11 : pi_quad_nxtdown = 2'b10;
      |    2'b10 : pi_quad_nxtdown = 2'b00;
      |  endcase
      |end
      |
      |always @(*) begin
      |  case (pi_quad)
      |    2'b00 : pi_quad_nxtup = 2'b10;
      |    2'b01 : pi_quad_nxtup = 2'b00;
      |    2'b11 : pi_quad_nxtup = 2'b01;
      |    2'b10 : pi_quad_nxtup = 2'b11;
      |  endcase
      |end
      |
      |endmodule
    """.stripMargin)
}


class wav_phase_detector(
  width   : Int,
  ileadq  : Int = 0
) extends BlackBox (Map(
  "WIDTH"   -> width,
  "ILEADQ"  -> ileadq
)) {
  val io = IO(new Bundle{
    val clk     = Input (Bool())
    val reset   = Input (Bool())
    val enable  = Input (Bool())
    val idata   = Input (UInt(width.W))
    val qdata   = Input (UInt(width.W))
    val up      = Output(Bool())                 
    val dn      = Output(Bool())
  })
}


class wav_sigdelt(width : Int = 7)(implicit p: Parameters)  extends BlackBox (Map("WIDTH" -> width)) with HasBlackBoxInline{
  val io = IO(new Bundle{
    val clk       = Input (Bool())
    val reset     = Input (Bool())
    val clear     = Input (Bool())
    val addin     = Input (UInt(width.W))
    val sumout    = Output(UInt(width.W))
    val polarity  = Output(Bool())
    val overflow  = Output(Bool())
  })
  
  var prefix = if(p(WavComponentPrefix) != None) p(WavComponentPrefix)+"_" else ""
  override val desiredName = s"${prefix}wav_sigdelt" 
  
  //setInline(s"${prefix}wav_sigdelt.v",
  setInline(s"${desiredName}.v",
    s"""module ${desiredName}
      | #(
      |   parameter                  WIDTH                    = 7,
      |   parameter                  REGISTER_OUTPUTS         = 0
      |  )
      |  (
      |   input  wire                clk,
      |   input  wire                reset,
      |   input  wire                clear,
      |   input  wire [WIDTH-1:0]    addin,
      |   output reg  [WIDTH-1:0]    sumout,
      |   output reg                 polarity,
      |   output reg                 overflow
      |  );
      |
      |  wire [WIDTH:0]              se_addin;
      |  wire [WIDTH:0]              se_rem;
      |  wire [WIDTH+1:0]            addout_in;
      |  reg  [WIDTH:0]              addout_ff;
      |
      |  assign se_addin   = {addin[WIDTH-1], addin};
      |  assign se_rem     = {addout_ff[WIDTH], addout_ff[WIDTH], addout_ff[WIDTH-2:0]};
      |  assign addout_in  = se_addin + se_rem;
      |
      |  always @(posedge clk or posedge reset) begin
      |    if (reset) begin
      |      addout_ff  <= {WIDTH+1{1'b0}};
      |      sumout     <= {WIDTH{1'b0}};
      |      polarity   <= 1'b0;
      |      overflow   <= 1'b0;
      |    end else begin
      |      addout_ff  <= clear ? {WIDTH+1{1'b0}} : addout_in[WIDTH:0];
      |      sumout     <= {addout_in[WIDTH], addout_in[WIDTH-2:0]};
      |      polarity   <= addout_in[WIDTH];
      |      overflow   <= addout_in[WIDTH]^addout_in[WIDTH-1];
      |    end
      |  end
      |
      |endmodule
    """.stripMargin)
}






class WavMultibitSync (dataWidth: Int)(implicit p: Parameters) extends RawModule{
  val w = IO(new Bundle{
    val clk     = Input (Clock())
    val reset   = Input (Reset())
    val inc     = Input (Bool())
    val data    = Input (UInt(dataWidth.W))
    val ready   = Output(Bool())
  })
  
  val r = IO(new Bundle{
    val clk     = Input (Clock())
    val reset   = Input (Reset())
    val inc     = Input (Bool())
    val data    = Output(UInt(dataWidth.W))
    val ready   = Output(Bool())
  })
  
  val mem       = withClockAndReset(w.clk, w.reset.asAsyncReset){RegInit(VecInit(Seq.fill(2)(0.U(dataWidth.W)))) }//RegInit(Vec(2, UInt(dataWidth.W)), 0.U)}
  
  val wptr_in   = Wire(Bool())
  val wptr      = withClockAndReset(w.clk, w.reset.asAsyncReset){RegNext(wptr_in, false.B)}
  val we        = Wire(Bool())
  
  
  val rptr_in   = Wire(Bool())
  val rptr      = withClockAndReset(r.clk, r.reset.asAsyncReset){RegNext(rptr_in, false.B)}
  val wptr_rclk = withClockAndReset(r.clk, r.reset.asAsyncReset){WavDemetReset(wptr)}
  
  
  val rptr_wclk = withClockAndReset(w.clk, w.reset.asAsyncReset){WavDemetReset(rptr)}
  
  wptr_in     := we ^ wptr
  w.ready     := ~(rptr_wclk ^ wptr)
  we          := w.inc & w.ready
  
  
  
  r.ready     := rptr ^ wptr_rclk
  rptr_in     := rptr ^ (r.inc & r.ready)
  r.data      := mem(rptr)
  
  when(we){
    mem(rptr) := w.data
  }
  
}
import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import chisel3.experimental.ChiselEnum
object WavMultibitSyncGen extends App {  
  implicit val p: Parameters = new BaseXbarConfig
  
  class myWrapper(dataWidth: Int)(implicit p: Parameters) extends RawModule{
    val w = IO(new Bundle{
      val clk     = Input (Bool())
      val reset   = Input (Bool())
      val inc     = Input (Bool())
      val data    = Input (UInt(dataWidth.W))
      val ready   = Output(Bool())
    })

    val r = IO(new Bundle{
      val clk     = Input (Bool())
      val reset   = Input (Bool())
      val inc     = Input (Bool())
      val data    = Output(UInt(dataWidth.W))
      val ready   = Output(Bool())
    })
    
    val mbs = Module(new WavMultibitSync(dataWidth))
    mbs.w.clk := w.clk.asClock
    mbs.w.reset := w.reset.asAsyncReset
    mbs.w.inc := w.inc
    mbs.w.data := w.data
    w.ready := mbs.w.ready
    
    mbs.r.clk := r.clk.asClock
    mbs.r.reset := r.reset.asAsyncReset
    mbs.r.inc := r.inc
    r.data := mbs.r.data
    r.ready := mbs.r.ready
  }
  
  val axiverilog = (new ChiselStage).emitVerilog(
    new myWrapper(4)(p),
     
    //args
    Array("--target-dir", "output/")
  )
}






class WavFIFOMem(dataWidth: Int, addrWidth: Int, name: String = "")(implicit p: Parameters) extends BlackBox(Map("DATA_SIZE" -> dataWidth, "ADDR_SIZE" -> addrWidth)) with HasBlackBoxInline{
  val io = IO(new Bundle{
    val wclk    = Input (Bool())
    val rclk    = Input (Bool())
    val wclken  = Input (Bool())
    val read_en = Input (Bool())
    val wreset  = Input (Bool())
    val wfull   = Input (Bool())
    val waddr   = Input (UInt(addrWidth.W))
    val raddr   = Input (UInt(addrWidth.W))
    val wdata   = Input (UInt(dataWidth.W))
    val rdata   = Output(UInt(dataWidth.W))
  })
  
  val depth = scala.math.pow(2, addrWidth).toInt
  var prefix = if(p(WavComponentPrefix) != None) p(WavComponentPrefix) else ""
  override val desiredName = if(name == "") s"${prefix}_WavFIFOMem" else s"${prefix}_${name}_${dataWidth}x${depth}"
  
  
  setInline(s"${desiredName}.v",
    s"""module ${desiredName} #(
      |  parameter                     DATA_SIZE        = ${dataWidth},
      |  parameter                     ADDR_SIZE        = ${addrWidth}
      |)(
      |  input  wire                   wclk,
      |  input  wire                   rclk,
      |  input  wire                   wclken,
      |  input  wire                   read_en,
      |  input  wire                   wreset,       //1/14/2018 added - sbridges
      |  input  wire                   wfull,
      |  input  wire [ADDR_SIZE-1:0]   waddr,
      |  input  wire [ADDR_SIZE-1:0]   raddr,
      |  input  wire [DATA_SIZE-1:0]   wdata,
      |  output wire [DATA_SIZE-1:0]   rdata
      |);
      |
      |localparam    DEPTH = 1<<ADDR_SIZE;
      |wire web;
      |wire reb;
      |
      |  reg [DATA_SIZE-1:0]   mem [0:DEPTH-1];
      |
      |  assign rdata  = mem[raddr];
      |
      |  integer i;
      |  always @(posedge wclk or posedge wreset) begin
      |    if(wreset) begin
      |      for(i = 0; i< (1<<ADDR_SIZE); i = i + 1) begin
      |        mem[i]      <= {DATA_SIZE{1'b0}};
      |      end
      |    end else begin
      |      if(wclken & ~wfull) begin
      |        mem[waddr]  <= wdata;
      |      end
      |    end
      |  end
      |
      |endmodule
    """.stripMargin)
}


class WavFIFOPtrLogic(isWritePtr: Int, addrWidth: Int)(implicit p: Parameters) extends BlackBox(Map("IS_WRITE_PTR" -> isWritePtr, "ADDR_SIZE" -> addrWidth)) with HasBlackBoxInline{
  val io = IO(new Bundle{
    val inc               = Input (Bool())
    val clk               = Input (Bool())
    val reset             = Input (Bool())
    val swi_almost_val    = Input (UInt(addrWidth.W))     
    val sync_ptr          = Input (UInt((addrWidth+1).W))
    val ptr               = Output(UInt((addrWidth+1).W))
    val bin_ptr           = Output(UInt((addrWidth+1).W))
    val diff              = Output(UInt((addrWidth+2).W))
    val addr              = Output(UInt(addrWidth.W))  
    val flag              = Output(Bool())
    val almost_fe         = Output(Bool())
    val half_full         = Output(Bool())
  })
  
  
  var prefix = if(p(WavComponentPrefix) != None) p(WavComponentPrefix)+"_" else ""
  override val desiredName = s"${prefix}WavFIFOPtrLogic" 
  
  setInline(s"${desiredName}.v",
    s"""
      |/*
      |* Based on sunburst-design.com's fifo design. Removed the need for two separate blocks
      |* by using generate statements to produce full/empty flag. Also changed reset to active high,
      |* because active low resets just confuse me
      |*/
      |
      |module ${desiredName} #(
      |  parameter                           IS_WRITE_PTR  = 1,      //Determines the output logic
      |  parameter                           ADDR_SIZE     = 4
      |)(
      |  input  wire                         inc,
      |  input  wire                         clk,
      |  input  wire                         reset,
      |  input  wire [ADDR_SIZE-1:0]         swi_almost_val,         //programmable value for almost full/empty. Set to the difference you wish
      |  input  wire [ADDR_SIZE:0]           sync_ptr,               //pointer from opposite logic block
      |  output reg  [ADDR_SIZE:0]           ptr,                    //this blocks gray-encoded pointer to opposite block
      |  output wire [ADDR_SIZE:0]           bin_ptr,                //this blocks binary pointer
      |  output wire [ADDR_SIZE+1:0]         diff,                   //difference between pointers
      |  output wire [ADDR_SIZE-1:0]         addr,                   //addr to memory
      |  output reg                          flag,                   //empty/full flag
      |  output reg                          almost_fe,              //almost full/empty flag, port representation is based on the setting
      |  output wire                         half_full               //1 when write pointer - read pointer is >= half of full val (you can think of it as half-empty if your one of those people)
      |);
      |
      |reg  [ADDR_SIZE:0]      bin;
      |wire [ADDR_SIZE:0]      graynext;
      |wire [ADDR_SIZE:0]      binnext;
      |
      |wire [ADDR_SIZE:0]      sync_bin;   //binary value of the sync ptr
      |//wire [ADDR_SIZE+1:0]    diff;
      |
      |always @(posedge clk or posedge reset) begin
      |  if(reset) begin
      |    bin           <= {ADDR_SIZE+1{1'b0}};
      |    ptr           <= {ADDR_SIZE+1{1'b0}};
      |  end else begin
      |    bin           <= binnext;
      |    ptr           <= graynext;
      |  end
      |end
      |
      |assign addr       = bin[ADDR_SIZE-1:0];
      |assign binnext    = bin + (inc & ~flag);
      |assign graynext   = (binnext>>1) ^ binnext;
      |
      |//gray2bin conversion for size checking
      |assign sync_bin[ADDR_SIZE:0] = sync_ptr[ADDR_SIZE:0] ^ {1'b0, sync_bin[ADDR_SIZE:1]};
      |
      |assign bin_ptr = bin;
      |
      |// Full/Empty logic generation, need to comeback to add in something that describes the almost full/empty cases
      |// Can't break out the flag register as the reset value is different depending on the mode
      |generate
      |  if(IS_WRITE_PTR == 1) begin : gen_full_logic
      |    //2 MSBs should not equal, lower bits should for full indication
      |    wire    full_int, half_full_int, almost_fe_int;
      |    reg     half_full_reg;
      |
      |    assign  diff            = bin + (~sync_bin + {{ADDR_SIZE-1{1'b0}}, 1'b1});
      |    assign  full_int        = (graynext == {~sync_ptr[ADDR_SIZE:ADDR_SIZE-1], sync_ptr[ADDR_SIZE-2:0]});
      |    assign  half_full_int   = (diff[ADDR_SIZE:0]   >= {2'b01, {ADDR_SIZE-1{1'b0}}});    //half of addr area
      |    assign  almost_fe_int   = (diff[ADDR_SIZE-1:0] >= swi_almost_val);                  //The higher you set this, the later it trips (stays high if full)
      |    assign  half_full       = half_full_reg;
      |
      |    always @(posedge clk or posedge reset) begin
      |      if(reset) begin
      |        flag          <= 1'b0;
      |        half_full_reg <= 1'b0;
      |        almost_fe     <= 1'b0;
      |      end else begin
      |        flag          <= full_int;
      |        half_full_reg <= half_full_int;
      |        almost_fe     <= almost_fe_int;
      |      end
      |    end
      |
      |  end else begin : gen_empty_logic
      |    //write pointer should equal read pointer for empty
      |    wire    empty_int, almost_fe_int;
      |
      |    assign empty_int        = (graynext == sync_ptr);
      |    assign half_full        = 1'b0;                                                     //half_full is invalid, so tieoff
      |    assign diff             = sync_bin + (~bin + {{ADDR_SIZE-1{1'b0}}, 1'b1});
      |    assign almost_fe_int    = (diff[ADDR_SIZE-1:0] <= swi_almost_val);
      |
      |    always @(posedge clk or posedge reset) begin
      |      if(reset) begin
      |        flag        <= 1'b1;
      |        almost_fe   <= 1'b1;
      |      end else begin
      |        flag        <= empty_int;
      |        almost_fe   <= almost_fe_int;
      |      end
      |    end
      |
      |  end
      |endgenerate
      |
      |endmodule
    """.stripMargin)
}



class WavFIFO(dataWidth: Int, addrWidth: Int, name: String = "", val withReplay: Boolean = false)(implicit p: Parameters) extends RawModule{
  
  val io = IO(new Bundle{
    val wclk                                  = Input (Bool())
    val wreset                                = Input (Bool())
    val winc                                  = Input (Bool())
    val rclk                                  = Input (Bool())
    val rreset                                = Input (Bool())
    val rinc                                  = Input (Bool())
    val rrevert                               = if(withReplay) Some(Input (Bool())) else None
    val rrevert_addr                          = if(withReplay) Some(Input (UInt((addrWidth+1).W))) else None
    val wdata                                 = Input (UInt(dataWidth.W))
    val rdata                                 = Output(UInt(dataWidth.W))
    val wfull                                 = Output(Bool())
    val rempty                                = Output(Bool())
    val rbin_ptr                              = Output(UInt((addrWidth+1).W))
    val rdiff                                 = Output(UInt((addrWidth+2).W))
    val wbin_ptr                              = Output(UInt((addrWidth+1).W))
    val wdiff                                 = Output(UInt((addrWidth+2).W))
    val swi_almost_empty                      = Input (UInt(addrWidth.W))
    val swi_almost_full                       = Input (UInt(addrWidth.W))
    val half_full                             = Output(Bool())
    val almost_empty                          = Output(Bool())
    val almost_full                           = Output(Bool())
  })
  
  
  val waddr     = Wire(UInt(addrWidth.W))
  val raddr     = Wire(UInt(addrWidth.W))
  val wptr      = Wire(UInt((addrWidth+1).W))
  val sync_wptr = Wire(UInt((addrWidth+1).W))
  val rptr      = Wire(UInt((addrWidth+1).W))
  val sync_rptr = Wire(UInt((addrWidth+1).W))
  
  sync_wptr     := withClockAndReset(io.rclk.asClock, io.rreset.asAsyncReset){WavDemetReset(wptr)}
  sync_rptr     := withClockAndReset(io.wclk.asClock, io.wreset.asAsyncReset){WavDemetReset(rptr)}
  
  val write_ptr_logic = Module(new WavFIFOPtrLogic(1, addrWidth))
  write_ptr_logic.io.inc            := io.winc
  write_ptr_logic.io.clk            := io.wclk
  write_ptr_logic.io.reset          := io.wreset
  write_ptr_logic.io.swi_almost_val := io.swi_almost_full
  write_ptr_logic.io.sync_ptr       := sync_rptr
  wptr                              := write_ptr_logic.io.ptr
  io.wbin_ptr                       := write_ptr_logic.io.bin_ptr
  io.wdiff                          := write_ptr_logic.io.diff
  waddr                             := write_ptr_logic.io.addr
  io.wfull                          := write_ptr_logic.io.flag
  io.almost_full                    := write_ptr_logic.io.almost_fe
  io.half_full                      := write_ptr_logic.io.half_full
  
  
  val read_ptr_logic = Module(new WavReplayFIFOPtrLogic(0, addrWidth))
  if(withReplay){ 
    read_ptr_logic.io.revert        := io.rrevert.get
    read_ptr_logic.io.revert_addr   := io.rrevert_addr.get
  }
  read_ptr_logic.io.inc             := io.rinc
  read_ptr_logic.io.clk             := io.rclk
  read_ptr_logic.io.reset           := io.rreset
  read_ptr_logic.io.swi_almost_val  := io.swi_almost_empty
  read_ptr_logic.io.sync_ptr        := sync_wptr
  rptr                              := read_ptr_logic.io.ptr
  io.rbin_ptr                       := read_ptr_logic.io.bin_ptr
  io.rdiff                          := read_ptr_logic.io.diff
  raddr                             := read_ptr_logic.io.addr
  io.rempty                         := read_ptr_logic.io.flag
  io.almost_empty                   := write_ptr_logic.io.almost_fe
  
  
  val mem = Module(new WavFIFOMem(dataWidth, addrWidth, name=name))
  mem.io.wclk      := io.wclk
  mem.io.rclk      := io.rclk
  mem.io.wclken    := io.winc
  mem.io.read_en   := ~io.rempty
  mem.io.wreset    := io.wreset
  mem.io.wfull     := io.wfull
  mem.io.waddr     := waddr
  mem.io.raddr     := raddr
  mem.io.wdata     := io.wdata
  io.rdata         := mem.io.rdata
  
}



/////////////////////////////////////
// NEW FIFO
/////////////////////////////////////

class WavReplayFIFOPtrLogic(isWritePtr: Int, addrWidth: Int)(implicit p: Parameters) extends BlackBox(Map("IS_WRITE_PTR" -> isWritePtr, "ADDR_SIZE" -> addrWidth)) with HasBlackBoxInline{
  val io = IO(new Bundle{
    val inc               = Input (Bool())
    val clk               = Input (Bool())
    val reset             = Input (Bool())
    val revert            = Input (Bool())
    val revert_addr       = Input (UInt((addrWidth+1).W))
    val swi_almost_val    = Input (UInt(addrWidth.W))     
    val sync_ptr          = Input (UInt((addrWidth+1).W))
    val ptr               = Output(UInt((addrWidth+1).W))
    val bin_ptr           = Output(UInt((addrWidth+1).W))
    val diff              = Output(UInt((addrWidth+2).W))
    val addr              = Output(UInt(addrWidth.W))  
    val flag              = Output(Bool())
    val almost_fe         = Output(Bool())
    val half_full         = Output(Bool())
  })
  
  var prefix = if(p(WavComponentPrefix) != None) p(WavComponentPrefix)+"_" else ""
  override val desiredName = s"${prefix}WavReplayFIFOPtrLogic" 
  
  setInline(s"${desiredName}.v",
    s"""
      |/*
      |* Based on sunburst-design.com's fifo design. Removed the need for two separate blocks
      |* by using generate statements to produce full/empty flag. Also changed reset to active high,
      |* because active low resets just confuse me
      |*/
      |
      |module ${desiredName} #(
      |  parameter                           IS_WRITE_PTR  = 1,      //Determines the output logic
      |  parameter                           ADDR_SIZE     = 4
      |)(
      |  input  wire                         inc,
      |  input  wire                         clk,
      |  input  wire                         reset,
      |  input  wire                         revert,
      |  input  wire [ADDR_SIZE:0]           revert_addr,
      |  input  wire [ADDR_SIZE-1:0]         swi_almost_val,         //programmable value for almost full/empty. Set to the difference you wish
      |  input  wire [ADDR_SIZE:0]           sync_ptr,               //pointer from opposite logic block
      |  output reg  [ADDR_SIZE:0]           ptr,                    //this blocks gray-encoded pointer to opposite block
      |  output wire [ADDR_SIZE:0]           bin_ptr,                //this blocks binary pointer
      |  output wire [ADDR_SIZE+1:0]         diff,                   //difference between pointers
      |  output wire [ADDR_SIZE-1:0]         addr,                   //addr to memory
      |  output reg                          flag,                   //empty/full flag
      |  output reg                          almost_fe,              //almost full/empty flag, port representation is based on the setting
      |  output wire                         half_full               //1 when write pointer - read pointer is >= half of full val (you can think of it as half-empty if your one of those people)
      |);
      |
      |reg  [ADDR_SIZE:0]      bin;
      |wire [ADDR_SIZE:0]      graynext;
      |wire [ADDR_SIZE:0]      binnext;
      |
      |wire [ADDR_SIZE:0]      sync_bin;   //binary value of the sync ptr
      |//wire [ADDR_SIZE+1:0]    diff;
      |
      |always @(posedge clk or posedge reset) begin
      |  if(reset) begin
      |    bin           <= {ADDR_SIZE+1{1'b0}};
      |    ptr           <= {ADDR_SIZE+1{1'b0}};
      |  end else begin
      |    bin           <= binnext;
      |    ptr           <= graynext;
      |  end
      |end
      |
      |assign addr       = bin[ADDR_SIZE-1:0];
      |assign binnext    = revert ? revert_addr : bin + (inc & ~flag);
      |assign graynext   = (binnext>>1) ^ binnext;
      |
      |//gray2bin conversion for size checking
      |assign sync_bin[ADDR_SIZE:0] = sync_ptr[ADDR_SIZE:0] ^ {1'b0, sync_bin[ADDR_SIZE:1]};
      |
      |assign bin_ptr = bin;
      |
      |// Full/Empty logic generation, need to comeback to add in something that describes the almost full/empty cases
      |// Can't break out the flag register as the reset value is different depending on the mode
      |generate
      |  if(IS_WRITE_PTR == 1) begin : gen_full_logic
      |    //2 MSBs should not equal, lower bits should for full indication
      |    wire    full_int, half_full_int, almost_fe_int;
      |    reg     half_full_reg;
      |
      |    assign  diff            = bin + (~sync_bin + {{ADDR_SIZE-1{1'b0}}, 1'b1});
      |    assign  full_int        = (graynext == {~sync_ptr[ADDR_SIZE:ADDR_SIZE-1], sync_ptr[ADDR_SIZE-2:0]});
      |    assign  half_full_int   = (diff[ADDR_SIZE:0]   >= {2'b01, {ADDR_SIZE-1{1'b0}}});    //half of addr area
      |    assign  almost_fe_int   = (diff[ADDR_SIZE-1:0] >= swi_almost_val);                  //The higher you set this, the later it trips (stays high if full)
      |    assign  half_full       = half_full_reg;
      |
      |    always @(posedge clk or posedge reset) begin
      |      if(reset) begin
      |        flag          <= 1'b0;
      |        half_full_reg <= 1'b0;
      |        almost_fe     <= 1'b0;
      |      end else begin
      |        flag          <= full_int;
      |        half_full_reg <= half_full_int;
      |        almost_fe     <= almost_fe_int;
      |      end
      |    end
      |
      |  end else begin : gen_empty_logic
      |    //write pointer should equal read pointer for empty
      |    wire    empty_int, almost_fe_int;
      |
      |    assign empty_int        = (graynext == sync_ptr);
      |    assign half_full        = 1'b0;                                                     //half_full is invalid, so tieoff
      |    assign diff             = sync_bin + (~bin + {{ADDR_SIZE-1{1'b0}}, 1'b1});
      |    assign almost_fe_int    = (diff[ADDR_SIZE-1:0] <= swi_almost_val);
      |
      |    always @(posedge clk or posedge reset) begin
      |      if(reset) begin
      |        flag        <= 1'b1;
      |        almost_fe   <= 1'b1;
      |      end else begin
      |        flag        <= empty_int;
      |        almost_fe   <= almost_fe_int;
      |      end
      |    end
      |
      |  end
      |endgenerate
      |
      |endmodule
    """.stripMargin)
}

// class WavReplayFIFO(dataWidth: Int, addrWidth: Int)(implicit p: Parameters) extends RawModule{
//   
//   val io = IO(new Bundle{
//     val wclk                                  = Input (Bool())
//     val wreset                                = Input (Bool())
//     val winc                                  = Input (Bool())
//     val rclk                                  = Input (Bool())
//     val rreset                                = Input (Bool())
//     val rinc                                  = Input (Bool())
//     val rrevert                               = Input (Bool())
//     val rrevert_addr                          = Input (UInt((addrWidth+1).W))
//     val wdata                                 = Input (UInt(dataWidth.W))
//     val rdata                                 = Output(UInt(dataWidth.W))
//     val wfull                                 = Output(Bool())
//     val rempty                                = Output(Bool())
//     val rbin_ptr                              = Output(UInt((addrWidth+1).W))
//     val rdiff                                 = Output(UInt((addrWidth+2).W))
//     val wbin_ptr                              = Output(UInt((addrWidth+1).W))
//     val wdiff                                 = Output(UInt((addrWidth+2).W))
//     val swi_almost_empty                      = Input (UInt(addrWidth.W))
//     val swi_almost_full                       = Input (UInt(addrWidth.W))
//     val half_full                             = Output(Bool())
//     val almost_empty                          = Output(Bool())
//     val almost_full                           = Output(Bool())
//   })
//   
//   
//   val waddr     = Wire(UInt(addrWidth.W))
//   val raddr     = Wire(UInt(addrWidth.W))
//   val wptr      = Wire(UInt((addrWidth+1).W))
//   val sync_wptr = Wire(UInt((addrWidth+1).W))
//   val rptr      = Wire(UInt((addrWidth+1).W))
//   val sync_rptr = Wire(UInt((addrWidth+1).W))
//   
//   sync_wptr     := withClockAndReset(io.rclk.asClock, io.rreset.asAsyncReset){WavDemetReset(wptr)}
//   sync_rptr     := withClockAndReset(io.wclk.asClock, io.wreset.asAsyncReset){WavDemetReset(rptr)}
//   
//   val write_ptr_logic = Module(new WavFIFOPtrLogic(1, addrWidth))
//   write_ptr_logic.io.inc            := io.winc
//   write_ptr_logic.io.clk            := io.wclk
//   write_ptr_logic.io.reset          := io.wreset
//   write_ptr_logic.io.swi_almost_val := io.swi_almost_full
//   write_ptr_logic.io.sync_ptr       := sync_rptr
//   wptr                              := write_ptr_logic.io.ptr
//   io.wbin_ptr                       := write_ptr_logic.io.bin_ptr
//   io.wdiff                          := write_ptr_logic.io.diff
//   waddr                             := write_ptr_logic.io.addr
//   io.wfull                          := write_ptr_logic.io.flag
//   io.almost_full                    := write_ptr_logic.io.almost_fe
//   io.half_full                      := write_ptr_logic.io.half_full
//   
//   
//   val read_ptr_logic = Module(new WavReplayFIFOPtrLogic(0, addrWidth))
//   read_ptr_logic.io.inc             := io.rinc
//   read_ptr_logic.io.clk             := io.rclk
//   read_ptr_logic.io.reset           := io.rreset
//   read_ptr_logic.io.swi_almost_val  := io.swi_almost_empty
//   read_ptr_logic.io.sync_ptr        := sync_wptr
//   
//   read_ptr_logic.io.revert          := io.rrevert
//   read_ptr_logic.io.revert_addr     := io.rrevert_addr
//   
//   rptr                              := read_ptr_logic.io.ptr
//   io.rbin_ptr                       := read_ptr_logic.io.bin_ptr
//   io.rdiff                          := read_ptr_logic.io.diff
//   raddr                             := read_ptr_logic.io.addr
//   io.rempty                         := read_ptr_logic.io.flag
//   io.almost_empty                   := write_ptr_logic.io.almost_fe
//   
//   
//   val mem = Module(new WavFIFOMem(dataWidth, addrWidth))
//   mem.io.wclk      := io.wclk
//   mem.io.rclk      := io.rclk
//   mem.io.wclken    := io.winc
//   mem.io.read_en   := ~io.rempty
//   mem.io.wreset    := io.wreset
//   mem.io.wfull     := io.wfull
//   mem.io.waddr     := waddr
//   mem.io.raddr     := raddr
//   mem.io.wdata     := io.wdata
//   io.rdata         := mem.io.rdata
//   
// }






/**
  *   RawModule implementation for DPRAM. This is what should be instantiated in your design
  */
class WlinkDRPRAM(dataWidth: Int, depth: Int, name: String) extends RawModule{
  
  val addrWidth = log2Up(depth)
  
  //This class is a RawModule as I want the user to be explicit regarding clock connections
  val io = IO(new Bundle{
    val wclk      = Input (Clock())
    val waddr     = Input (UInt(addrWidth.W))
    val wen       = Input (Bool())
    val wdata     = Input (UInt(dataWidth.W))
    
    val rclk      = Input (Clock())
    val raddr     = Input (UInt(addrWidth.W))
    val ren       = Input (Bool())
    val rdata     = Output(UInt(dataWidth.W))
  })
  
  val dpram = Module(new WlinkDPRAMBB(dataWidth=dataWidth, depth=depth, addrWidth=addrWidth, name=name))
  
  dpram.io.clk_0    := io.wclk
  dpram.io.addr_0   := io.waddr
  dpram.io.en_0     := io.wen
  dpram.io.we_0     := io.wen
  dpram.io.wdata_0  := io.wdata
  
  dpram.io.clk_1    := io.rclk
  dpram.io.addr_1   := io.raddr
  dpram.io.en_1     := io.ren
  dpram.io.we_1     := false.B
  dpram.io.wdata_1  := 0.U
  
  io.rdata          := dpram.io.rdata_1
  
}


/**
  *   BlackBox implementation for DPRAM
  */
class WlinkDPRAMBB(dataWidth: Int, depth: Int, addrWidth: Int, name: String) extends BlackBox(Map("DWIDTH" -> dataWidth, "SIZE" -> depth, "AWIDTH" -> addrWidth)) with HasBlackBoxInline{
    
  val io = IO(new Bundle{
    val clk_0    = Input (Clock())
    val addr_0   = Input (UInt(addrWidth.W))     
    val en_0     = Input (Bool())
    val we_0     = Input (Bool())
    val wdata_0  = Input (UInt(dataWidth.W))     
    val rdata_0  = Output(UInt(dataWidth.W))     
    val clk_1    = Input (Clock())
    val addr_1   = Input (UInt(addrWidth.W))     
    val en_1     = Input (Bool())
    val we_1     = Input (Bool())
    val wdata_1  = Input (UInt(dataWidth.W))     
    val rdata_1  = Output(UInt(dataWidth.W))  
  })
  
  //Forces this BlackBox to have a particular Module Name. We do this to make memory
  //replacement easier.
  override val desiredName = s"${name}_${dataWidth}x${depth}"
  
  
  //TODO : Add an elab artifact to get memories post generations
  
   setInline(s"${desiredName}.v",
    s"""module ${desiredName} #(
      |   parameter DWIDTH = 32,              // Data width
      |   parameter SIZE   = 256,             // RAM size in DWIDTHs
      |   parameter AWIDTH = 8//     // Address width
      |) (
      |   input  wire               clk_0,
      |   input  wire [AWIDTH-1:0]  addr_0,
      |   input  wire               en_0,
      |   input  wire               we_0,
      |   input  wire [DWIDTH-1:0]  wdata_0,
      |   output wire [DWIDTH-1:0]  rdata_0,
      |
      |   input  wire               clk_1,
      |   input  wire [AWIDTH-1:0]  addr_1,
      |   input  wire               en_1,
      |   input  wire               we_1,
      |   input  wire [DWIDTH-1:0]  wdata_1,
      |   output wire [DWIDTH-1:0]  rdata_1
      |);
      |
      |reg   [DWIDTH-1:0] mem [SIZE-1:0];
      |wire  write_0, read_0;
      |wire  write_1, read_1;
      |reg   [AWIDTH-1:0] addr_0_reg, addr_1_reg;
      |
      |assign write_0 = en_0 &  we_0;
      |assign read_0  = en_0 & ~we_0;
      |
      |integer i;
      |always @(posedge clk_0) begin
      |  if (write_0) begin
      |    mem[addr_0] <= wdata_0;
      |  end
      |end
      |
      |always @(posedge clk_0) begin
      |  if (read_0) begin
      |    addr_0_reg <= addr_0;
      |  end
      |end
      |
      |assign rdata_0 = read_0 ? mem[addr_0] : mem[addr_0_reg];
      |
      |assign write_1 = en_1 &  we_1;
      |assign read_1  = en_1 & ~we_1;
      |
      |integer j;
      |// always @(posedge clk_1) begin
      |//   if (write_1) begin
      |//     mem[addr_1] <= wdata_1;
      |//   end
      |// end
      |
      |always @(posedge clk_1) begin
      |  if (read_1) begin
      |    addr_1_reg <= addr_1;
      |  end
      |end
      |
      |assign rdata_1 = read_1 ? mem[addr_1] : mem[addr_1_reg];
      |
      |endmodule
    """.stripMargin)
    
}
