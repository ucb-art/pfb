// See LICENSE for license details.

package pfb

import cde.Parameters
import chisel3._
import dsptools._
import dsptools.numbers._
import dspjunctions._
import dspblocks._

class LazyPFBBlock[T <: Data : Real]()(implicit p: Parameters) extends LazyDspBlock()(p) {
  def controls = Seq()
  def statuses = Seq()

  lazy val module = Module(new PFBBlock[T](this))

}

class PFBBlock[T <: Data : Real](outer: LazyDspBlock)(implicit p: Parameters)
  extends GenDspBlock[T, T](outer)(p) with HasPFBParameters[T] {
  val baseAddr = BigInt(0)

  val module = Module(new PFB(genIn(), Some(genOut()), genTap, pfbConfig))
  
  module.io.data_in <> unpackInput(lanesIn, genIn())
  unpackOutput(lanesOut, genOut()) <> module.io.data_out

}
