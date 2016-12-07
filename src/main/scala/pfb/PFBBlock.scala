// See LICENSE for license details.

package pfb

import cde.Parameters
import chisel3._
import dsptools._
import dsptools.numbers._

class PFBBlock[T <: Data : Real](implicit p: Parameters)
  extends GenDspBlock[T, T]()(p) with HasPFBParameters[T] {
  val baseAddr = BigInt(0)

  addControl("pfbControl")
  val module = Module(new PFB(genIn(), Some(genOut()), genTap, pfbConfig))
  
  module.io.data_in <> unpackInput(lanesIn, genIn())
  unpackOutput(lanesOut, genOut()) <> module.io.data_out

}
