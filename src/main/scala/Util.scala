package wav.common

//import Chisel._
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.ChiselStage

import freechips.rocketchip.config.{Parameters, Field, Config}
import freechips.rocketchip.util._


/** 
  *   Various utilities we need during generation
  */
object WavConfigUtil {

  def getConfig(fullConfigClassNames: Seq[String]): Config = {
    new Config(fullConfigClassNames.foldRight(Parameters.empty) { case (currentName, config) =>
      val currentConfig = try {
        Class.forName(currentName).newInstance.asInstanceOf[Config]
      } catch {
        case e: java.lang.ClassNotFoundException =>
          import Chisel.throwException
          throwException(s"""Unable to find part "$currentName" from "$fullConfigClassNames", did you misspell it or specify the wrong package path?""", e)
      }
      currentConfig ++ config
    })
  }

}
