package pfb

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.core.requireIsChiselType
import chisel3.util._
import dspblocks._
import dsptools.numbers._
//import dsptools.numbers.implicits._
import freechips.rocketchip.amba.axi4.{AXI4BlindInputNode, AXI4MasterParameters, AXI4MasterPortParameters}
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.coreplex.BaseCoreplexConfig
import freechips.rocketchip.diplomacy._

import scala.collection.Seq


class PFBBlind[T <: Data : Ring](val pfbParams: PFBParams[T],
                                     blindNodes: DspBlock.AXI4BlindNodes)(
                                     implicit p: Parameters) extends LazyModule {

  val streamIn  = blindNodes.streamIn()
  val streamOut = blindNodes.streamOut()
  val mem       = blindNodes.mem()

  val pfb = LazyModule(new PFB(pfbParams))
  pfb.streamNode := streamIn
  streamOut := pfb.streamNode
  pfb.mem.get := mem

  lazy val module = new PFBBlindModule(this)
}

class PFBBlindModule[T <: Data : Ring](val outer: PFBBlind[T]) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val in = outer.streamIn.bundleOut
    val out = outer.streamOut.bundleIn
    val mem = outer.mem.bundleOut
  })

}

