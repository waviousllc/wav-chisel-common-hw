//package wav.common
package freechips.rocketchip.diplomacy

// Custom Register Nodes

import Chisel._
import chisel3.RawModule
import firrtl.annotations.ModuleName
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.model.{OMRegister, OMRegisterMap}
import freechips.rocketchip.regmapper._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.ahb._
import freechips.rocketchip.amba.apb._
import scala.math.{min,max}



import freechips.rocketchip.diplomaticobjectmodel.model._

/**
  *
  */

import java.io.{File, FileWriter}
object GenElabArts {
  def gen(prefix: String, outputDir: String = ".") {
    ElaborationArtefacts.files.foreach { case (extension, contents) =>
      //println(s"ext - ${extension} : cont - ${contents}")
      val f = new File(s"${outputDir}/", s"${prefix}.${extension}")
      val fw = new FileWriter(f)
      fw.write(contents())//note the ()
      fw.close
    }
    
    //WavRegAnnos.genSystemDVFiles(prefix, outputDir)
  }
}


/**
  *   Containts the register info for each LazyModule that includes a register block
  *   Prefixing and base addressing is taken into account so no need to create a larger
  *   level BLK file that does that.
  *
  */
object WavRegAnnos {
  
  var registerInfo = new StringBuilder("")
  //ListMap to maintain order
  // Address is key, (module hier, register info (if Chisel), custom register type, noRegTest for custom types)
  var registerInfoMap = scala.collection.immutable.ListMap[BigInt, (String, String, Boolean, Boolean)]()
  
  
  def getPath(): String = {
    var path = ""
    val module = Module.currentModule.get
    var lmod = Module.currentModule.get.asInstanceOf[LazyModuleImp].wrapper
    var top  = false
    var onlyTop = true
    
    while(!top){
      //this parent check is the reason we have to declare this as part of the diplomacy package
      //recursively goes through and captures all of the hier
      lmod.parent match {
        case None     => {
          top = true
        }
        case Some(lm) => {
          path = lmod.name + "_" + path
          lmod = lm
          onlyTop = false
        }
      }
    }
    
    if(onlyTop) path = "top_"
    path = path.toUpperCase()
    path = path.dropRight(1)    //remove the trailing '_'
    path
  }
  
  def genDVFile(baseAddress: BigInt, noRegTest: Boolean, mapping: RegField.Map*) = {
    
    //Get the path name of the module in which this was called.
    //We walk up the parents
    var path = WavRegAnnos.getPath()
    
    
    
    val regFieldSers = mapping.flatMap {
      case (byteOffset, seq) =>
        seq.map(_.width).scanLeft(0)(_ + _).zip(seq).map { case (bitOffset, regField) =>
          GenRegDescsAnno.makeRegMappingSer(
            Module.currentModule.get.asInstanceOf[RawModule],//rawModule,
            "blah",//moduleName,
            baseAddress,
            regField.width,
            byteOffset,
            bitOffset,
            regField
          )
        }
    }
    
    val sb = new StringBuilder("")
    
    sb.append("\n\n\n")
    sb.append(s"# Register Info for ${path}\n")
    sb.append(s"# REGNAME         TYPE            ADDR                      Description\n")
    sb.append(s"# BFNAME          WIDTH           LSB    RESET    TYPE      Description\n")
    
    var curreg = ""
    
    regFieldSers.foreach {rf =>
      
      if(rf.group != curreg){
        //New register
        curreg = rf.group
        val hexaddr = (/*baseAddress +*/ BigInt(rf.byteOffset.replace("0x",""), 16)).toString(16)
        //TODO: Need to see how we can do this access type a little better. It may not really matter since we do it per bitfield
        val nrt = if(noRegTest) "<NO_REG_TEST>" else ""
        sb.append("\n%-60s  %-4s 'h%-20s <DESC>%s<\\DESC>%s\n".format(curreg.toUpperCase(), "RW", hexaddr, rf.groupDesc, nrt))
      }
      
      //Bitfield
      val resetVal = rf.resetValue.toString(16)
      val accessType = rf.accessType match {
        case "R" => "RO"
        case "W" => "WO"
        //case _ => rf.accessType
        case _ => { 
          rf.wrType match {
            case "Some(ONE_TO_CLEAR)" => "W1C"
            case _ => rf.accessType
          }
        }
      }
      
      sb.append("%-36s %-4s %-4s %s'h%-10s  %-26s  <DESC>%s<\\DESC>\n".format(rf.name.toUpperCase(), rf.bitWidth, rf.bitOffset, rf.bitWidth, resetVal, accessType, rf.desc))
      
    }
    
    
    //registerInfoMap = registerInfoMap + (path -> (baseAddress, sb.toString))
    registerInfoMap = registerInfoMap + (baseAddress -> (path, sb.toString, false, false))
    
  }
  
  
  def addNonChiselRegFile(baseAddress: BigInt, filename: String, noRegTest: Boolean = false): Unit = {
    val path = WavRegAnnos.getPath()
    registerInfoMap = registerInfoMap + (baseAddress -> (path, filename, true, noRegTest))
  }
  
  
  def genSystemDVFiles(filePrefix: String, outputDir: String, envVar: String, offsetAdjustment: BigInt = 0): Unit = {
    for((addr, info) <- registerInfoMap){
      val f = new File(s"${outputDir}/", s"${info._1}.regs.txt")
      val fw = new FileWriter(f)
      fw.write(info._2.toString)
      fw.close
    }
    
    val f = new File(s"${outputDir}/", s"${filePrefix}.regs.txt")
    val fw = new FileWriter(f)
    for((addr, info) <- registerInfoMap){
      val newBase = addr + offsetAdjustment
      if(info._3){
        // File       Instance      Offset
        fw.write("%-60s %-60s %s  %s\n".format(info._2, info._1, "0x"+newBase.toString(16), if(info._4) "-nrt" else ""))
      } else {
        fw.write("DV: %-60s %-60s %s\n".format("${"+envVar+"}/"+info._1+".regs.txt", info._1, "0x"+newBase.toString(16)))
      }
    }
    fw.close
    
  }
  
}





/**
  *   APB
  *
  */

//case class WavAPBRegisterNode(address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false)(implicit valName: ValName)
case class WavAPBRegisterNode(address: Seq[AddressSet], device: Device, concurrency: Int = 0, beatBytes: Int = 4, undefZero: Boolean = true, executable: Boolean = false, noRegTest: Boolean = false)(implicit valName: ValName)
  extends SinkNode(APBImp)(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      //address       = Seq(address),
      address       = address,
      executable    = executable,
      supportsWrite = true,
      supportsRead  = true)),
    beatBytes  = beatBytes)))
{
  //require (address.contiguous)
  // Modified the following to match the WavTLRegisterRouter
  //println(s"---${address}")
  val size = 1 << log2Ceil(1 + address.map(_.max).max - address.map(_.base).min)
  require (size >= beatBytes)
  address.foreach { case a =>
    require (a.widen(size-1).base == address.head.widen(size-1).base,
      s"TLRegisterNode addresses (${address}) must be aligned to its size ${size}")
  }
  //println(s"size---${size}")

  // Calling this method causes the matching APB bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) = {
    val (apb, _) = this.in(0)

    //val indexBits = log2Up((address.mask+1)/beatBytes)
    val indexBits = log2Up((address.head.mask+1)/beatBytes)
    //val params = RegMapperParams(indexBits, beatBytes)
    val params = RegMapperParams(log2Up(size/beatBytes), beatBytes)
    val in = Wire(Decoupled(new RegMapperInput(params)))
    val out = RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*)

    // Only send the request to the RR once
    val taken = RegInit(Bool(false))
    when (in.fire())  { taken := Bool(true)  }
    when (out.fire()) { taken := Bool(false) }

    in.bits.read  := !apb.pwrite
    in.bits.index := apb.paddr >> log2Ceil(beatBytes)
    in.bits.data  := apb.pwdata
    in.bits.mask  := Mux(apb.pwrite, apb.pstrb, UInt((1<<beatBytes) - 1))

    in.valid := apb.psel && !taken
    out.ready := apb.penable

    apb.pready  := out.valid
    apb.pslverr := Bool(false)
    apb.prdata  := out.bits.data
    
    WavRegAnnos.genDVFile(address.head.base, noRegTest, mapping:_*)
  }
}

/**
  *   TileLink
  *
  */
case class WavTLRegisterNode(
    address:     Seq[AddressSet],
    device:      Device,
    deviceKey:   String  = "reg/control",
    concurrency: Int     = 0,
    beatBytes:   Int     = 4,
    undefZero:   Boolean = true,
    executable:  Boolean = false)(
    implicit valName: ValName)
  extends SinkNode(TLImp)(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(
      address            = address,
      resources          = Seq(Resource(device, deviceKey)),
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    minLatency = min(concurrency, 1)))) with TLFormatNode // the Queue adds at most one cycle
{
  val size = 1 << log2Ceil(1 + address.map(_.max).max - address.map(_.base).min)
  require (size >= beatBytes)
  address.foreach { case a =>
    require (a.widen(size-1).base == address.head.widen(size-1).base,
      s"TLRegisterNode addresses (${address}) must be aligned to its size ${size}")
  }

  // Calling this method causes the matching TL2 bundle to be
  // configured to route all requests to the listed RegFields.
  def regmap(mapping: RegField.Map*) : OMRegisterMap = {
    val (bundleIn, edge) = this.in(0)
    val a = bundleIn.a
    val d = bundleIn.d

    val fields = TLRegisterRouterExtraField(edge.bundle.sourceBits, edge.bundle.sizeBits) +: a.bits.params.echoFields
    val params = RegMapperParams(log2Up(size/beatBytes), beatBytes, fields)
    val in = Wire(Decoupled(new RegMapperInput(params)))
    in.bits.read  := a.bits.opcode === TLMessages.Get
    in.bits.index := edge.addr_hi(a.bits)
    in.bits.data  := a.bits.data
    in.bits.mask  := a.bits.mask
    in.bits.extra :<= a.bits.echo

    val a_extra = in.bits.extra(TLRegisterRouterExtra)
    a_extra.source := a.bits.source
    a_extra.size   := a.bits.size

    // Invoke the register map builder
    val out = RegMapper(beatBytes, concurrency, undefZero, in, mapping:_*)

    // No flow control needed
    in.valid  := a.valid
    a.ready   := in.ready
    d.valid   := out.valid
    out.ready := d.ready

    // We must restore the size to enable width adapters to work
    val d_extra = out.bits.extra(TLRegisterRouterExtra)
    d.bits := edge.AccessAck(toSource = d_extra.source, lgSize = d_extra.size)

    // avoid a Mux on the data bus by manually overriding two fields
    d.bits.data := out.bits.data
    d.bits.echo :<= out.bits.extra
    d.bits.opcode := Mux(out.bits.read, TLMessages.AccessAckData, TLMessages.AccessAck)

    // Tie off unused channels
    bundleIn.b.valid := Bool(false)
    bundleIn.c.ready := Bool(true)
    bundleIn.e.ready := Bool(true)
    
    WavRegAnnos.genDVFile(address.head.base, false, mapping:_*)
    //genRegDescsJson(mapping:_*)
    genOMRegMap(mapping:_*)
  }

  def genOMRegMap(mapping: RegField.Map*): OMRegisterMap = {
    OMRegister.convert(mapping = mapping:_*)
  }

  def genRegDescsJson(mapping: RegField.Map*) {
    // Dump out the register map for documentation purposes.
    val base = address.head.base
    val baseHex = s"0x${base.toInt.toHexString}"
    val name = s"deviceAt${baseHex}" //TODO: It would be better to name this other than "Device at ...."
    val json = GenRegDescsAnno.serialize(base, name, mapping:_*)
    var suffix = 0
    while( ElaborationArtefacts.contains(s"${baseHex}.${suffix}.regmap.json")) {
      suffix = suffix + 1
    }
    ElaborationArtefacts.add(s"${baseHex}.${suffix}.regmap.json", json)
    
//     println("in my custom reg node")
// 
     val module = Module.currentModule.get.asInstanceOf[RawModule]
//     val lmodule = Module.currentModule.get.asInstanceOf[LazyModuleImp]
//     println(s"module: ${module}")
//     println(s"module.name: ${module.name}")
//     println(s"module.wname: ${lmodule.wrapper.name}")
//     
//     println(s"moudle.parent : ${module.parentModName}")
//     
//     WavRegAnnos.genDVFile(base, mapping:_*)
    
    GenRegDescsAnno.anno(
      module,
      base,
      mapping:_*)

  }
  
}
