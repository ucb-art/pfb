// See LICENSE for license details.

package pfb

import cde.Parameters
import chisel3._
import dsptools._
import dsptools.numbers._
import dspjunctions._
import dspblocks._

class PFBBlock[T <: Data : Ring]()(implicit p: Parameters) extends DspBlock()(p) {
  def controls = Seq()
  def statuses = Seq()

  lazy val module = new PFBBlockModule[T](this)

  addStatus("Data_Set_End_Status")
  addControl("Data_Set_End_Clear", 0.U)

  addControl("Wrapback", 0.U)

}

class PFBBlockModule[T <: Data : Ring](outer: DspBlock)(implicit p: Parameters)
  extends GenDspBlockModule[T, T](outer)(p) with HasPFBParameters[T] {

  val module = Module(new PFB(genIn(), Some(genOut()), genTap, convert, pfbConfig))
  
  module.io.data_in <> unpackInput(lanesIn, genIn())
  unpackOutput(lanesOut, genOut()) <> module.io.data_out

  status("Data_Set_End_Status") := module.io.data_set_end_status
  module.io.data_set_end_clear := control("Data_Set_End_Clear")

  IPXactComponents._ipxactComponents += DspIPXact.makeDspBlockComponent(baseAddr)
}
