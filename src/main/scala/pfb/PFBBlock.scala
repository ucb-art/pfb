// See LICENSE for license details.

package pfb

import cde.Parameters
import chisel3._
import dsptools._
import dsptools.numbers._
import dspjunctions._
import dspblocks._

class PFBBlock[T <: Data : Ring : ConvertableTo]()(implicit p: Parameters) extends DspBlock()(p) {
  def controls = Seq()
  def statuses = Seq()

  lazy val module = new PFBBlockModule[T](this)

}

class PFBBlockModule[T <: Data : Ring : ConvertableTo](outer: DspBlock)(implicit p: Parameters)
  extends GenDspBlockModule[T, T](outer)(p) with HasPFBParameters[T] {
  val module = Module(new PFB(genIn(), Some(genOut()), genTap, pfbConfig))
  
  module.io.data_in <> unpackInput(lanesIn, genIn())
  unpackOutput(lanesOut, genOut()) <> module.io.data_out

  IPXactComponents._ipxactComponents += DspIPXact.makeDspBlockComponent
}
