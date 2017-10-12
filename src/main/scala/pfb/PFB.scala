package pfb

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.core.requireIsChiselType
import chisel3.util._
import dspblocks._
import dsptools._
import dsptools.numbers._
//import dsptools.numbers.implicits._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.coreplex.BaseCoreplexConfig
import freechips.rocketchip.diplomacy._

import scala.collection.Seq

class PFB[T <: Data : Ring](val pfbParams: PFBParams[T])
                                (implicit p: Parameters) extends DspBlock[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle]
  with AXI4DspBlock with AXI4HasCSR {

  // Add CSRs
  addControl("ctrl")
  addStatus("stat", 0)

  // identity node means inputs and outputs are the same
  // adapter node means different IO parameters 
  val streamNode = AXI4StreamIdentityNode() //streamNodeOpt.getOrElse(streamInOpt.get)

  val csrSize            : Int        = 8 * csrMap.size
  override def beatBytes : Int        = 8
  override def csrAddress: AddressSet = AddressSet(0x0, 0xff)
  override def csrBase   : Int        = 0

  makeCSRs()

  lazy val module = new PFBModule(this)
}

class PFBModule[T <: Data : Ring](outer: PFB[T]) extends LazyModuleImp(outer) {

  val (inx, _)  = outer.streamNode.in.unzip
  val (outx, _) = outer.streamNode.out.unzip
  val mem       = outer.mem.map(_.in.map(_._1))
  val config    = outer.pfbParams

  // get fields from outer class
  val genInVec     : Vec[T]   = Vec(config.lanes, config.genIn)
  val genOutVec    : Vec[T]   = Vec(config.lanes, config.genOut.getOrElse(config.genIn))

  val csrs = outer.csrs.module.io.csrs

  // cast input to T
  val in               = inx(0)
  val in_data: Vec[T]  = in.bits.data.asTypeOf(genInVec)
  val out              = outx(0)
  val out_data: Vec[T] = out.bits.data.asTypeOf(genOutVec)

  // split window up into config.lanes different sub-windows
  val groupedWindow = (0 until config.lanes).map(
    config.window.drop(_).grouped(config.lanes).map(_.head).toSeq
  )

  // resyncrhonize when valid goes high
  val valid_delay = Reg(next=in.valid)
  val cycleTime = config.outputWindowSize / config.lanes
  val counter = CounterWithReset(true.B, cycleTime, in.bits.last, ~valid_delay & in.valid)._1

  // user-defined latency, account for pipeline delays automatically
  val latency = config.processingDelay + config.multiplyPipelineDepth + config.outputPipelineDepth
  out.bits.last := ShiftRegisterWithReset(counter === (cycleTime-1).U, latency, 0.U) // don't let valid gate sync
  out.valid := ShiftRegisterWithReset(in.valid, latency, 0.U)

  // feed in zeros when invalid
  val in2 = Wire(genInVec)
  when (in.valid) {
    in2 := in_data
  } .otherwise {
    in2 := Vec.fill(config.lanes)(Ring[T].zero)
  }

  // create config.lanes filters and connect them up
  val filters = groupedWindow.map( taps => Module(new PFBLane(config.genIn, config.genOut, config.genTap, taps, cycleTime, config.convert, config)))
  filters.zip(in2).foreach( { case (filt, port) => filt.io.data_in := port } )
  filters.zip(out_data).foreach( { case (filt, port) => port := ShiftRegister(filt.io.data_out, config.outputPipelineDepth) } )
  filters.foreach (f => {
    f.io.count := counter
  })

  out.bits.data := out_data.asUInt
}


/**
  * A single lane of a [[PFB]]. A full PFB will include >= 1 lanes operating in parallel.
  * The PFB consists of `delay` parallel FIR filters, which means `taps.length / delay`
  * windows are added together. As a result, `delay` is required to divide `taps.length`
  * evenly.
  *
  * The FIR filter is implemented in transposed form. Delays are implemented with chisel
  * `Mem`s which may be implemented with SRAMs.
  * @param genIn Type generator for input
  * @param genOut Optional type generator for output, `genIn` by default.
  * @param genTap Optional type generator for window coefficients, `genIn` by default.
  * @param coeffs Seq of window coefficients. `V` must be a type that is convertable to `T`
  * @param delay Size of the delays in the PFB. This also ends up being the number of parallel
  *              FIR filters.
  * @tparam T
  * @tparam V
  */
class PFBLane[T<:Data:Ring](
  genIn: => T,
  genOut: => Option[T] = None,
  genTap: => Option[T] = None,
  val coeffs: Seq[Double],
  val delay: Int,
  val convert: Double => T,
  val config: PFBParams[T]

) extends Module {
  val io = IO(new Bundle {
    val data_in  = Input(genIn)
    val data_out = Output(genOut.getOrElse(genIn))
    val count = Input(UInt(log2Up(config.outputWindowSize / config.lanes).W))
  })

  require(coeffs.length % delay == 0)

  //println("This lane has coefficients:")
  //val temp = coeffs.map(x => x*math.pow(2,17))
  //println(temp.toArray.deep.mkString("\n"))

  val coeffsGrouped  = coeffs.grouped(delay).toSeq
  //val coeffsReversed = coeffsGrouped.map(_.reverse).reverse
  val coeffsWire     = coeffsGrouped.map(tapGroup => {
    val coeffWire = Wire(Vec(tapGroup.length, genTap.getOrElse(genIn)))
    coeffWire.zip(tapGroup).foreach({case (t,d) => t := convert(d)})
    coeffWire
  })

  val products = coeffsWire.map(tap => ShiftRegister(DspContext.withTrimType(NoTrim) {tap(io.count) * io.data_in}, config.multiplyPipelineDepth))

  val result = products.reduceLeft { (prev:T, prod:T) =>
     DspContext.withTrimType(NoTrim) {ShiftRegister(prev, delay) + prod}
  }

  io.data_out := result
}



case class PFBParams[T <: Data]
(
  // generic
  genIn: T,
  genOut: Option[T] = None,
  name: String = "pfb",

  // custom
  windowFunc: WindowConfig => Seq[Double],
  numTaps: Int,
  outputWindowSize: Int,
  scale: Double = 1.0,
  offset: Double = 0.0,
  lanes: Int = 8,
  outputPipelineDepth: Int = 1,
  multiplyPipelineDepth: Int = 1,
  genTap: Option[T] = None,
  convert: Double => T

) {
  requireIsChiselType(genIn,  s"genIn ($genIn) must be chisel type")
  genOut.foreach(g => requireIsChiselType(g, s"genOut ($g) must be chisel type"))

  val processingDelay = (numTaps-1)*(outputWindowSize/lanes)

  // various checks for validity
  val window = windowFunc(new WindowConfig(numTaps, outputWindowSize, scale, offset))
  val windowSize = window.length
  require(numTaps > 0, "Must have more than zero taps")
  require(outputWindowSize > 0, "Output window must have size > 0")
  require(outputWindowSize % lanes == 0, "Number of parallel inputs must divide the output window size")
  require(windowSize > 0, "PFB window must have > 0 elements")
  require(windowSize == numTaps * outputWindowSize, "windowFunc must return a Seq() of the right size")
}

/**
  * Object used by [[PFB]] to generate window from [[PFBConfig]] parameters
  * @param numTaps
  * @param outputWindowSize
  * @param scale
  * @param offset
  */
case class WindowConfig(
                         numTaps: Int,
                         outputWindowSize: Int,
                         scale: Double,
                         offset: Double
)
