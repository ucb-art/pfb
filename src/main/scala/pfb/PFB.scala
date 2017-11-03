package pfb

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.core.requireIsChiselType
import chisel3.internal.firrtl.KnownBinaryPoint
import chisel3.util._
import dspblocks._
import dsptools._
import craft._
import dsptools.numbers._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.coreplex.BaseCoreplexConfig
import freechips.rocketchip.diplomacy._

import scala.collection.Seq
import scala.math.{max, min, pow, abs, round}

class PFB[T <: Data : Real](val config: PFBParams[T])
                                (implicit p: Parameters) extends DspBlock[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle]
  with AXI4DspBlock with AXI4HasCSR {

  // Add CSRs
  addControl("ctrl")
  addStatus("stat", 0)

  // identity node means inputs and outputs are the same
  // adapter node means different IO parameters 
  // val streamNode = AXI4StreamIdentityNode() //streamNodeOpt.getOrElse(streamInOpt.get)
  val streamNode = AXI4StreamAdapterNode(
    AXI4StreamAdapterNode.widthAdapter(_, _ + config.bytesAdded),
    {s: AXI4StreamSlavePortParameters => s}
  )

  val csrSize            : Int        = 8 * csrMap.size
  override def beatBytes : Int        = 8
  override def csrAddress: AddressSet = AddressSet(0x0, 0xff)
  override def csrBase   : Int        = 0

  makeCSRs()

  lazy val module = new PFBModule(this)
}

class PFBModule[T <: Data : Real](outer: PFB[T]) extends LazyModuleImp(outer) {

  val (inx, _)  = outer.streamNode.in.unzip
  val (outx, _) = outer.streamNode.out.unzip
  val mem       = outer.mem.map(_.in.map(_._1))
  val config    = outer.config

  // get fields from outer class
  val genInVec     : Vec[T]   = Vec(config.lanes, config.genInInternal)
  val genOutVec    : Vec[T]   = Vec(config.lanes, config.genOutInternal)

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

  // delay valid and last signals
  out.bits.last := ShiftRegisterWithReset(counter === (cycleTime-1).U, config.processingDelay, 0.U) // don't let valid gate sync
  out.valid := ShiftRegisterWithReset(in.valid, config.processingDelay, 0.U)

  // feed in zeros when invalid
  val in2 = Wire(genInVec)
  when (in.valid) {
    in2 := in_data
  } .otherwise {
    in2 := Vec.fill(config.lanes)(Real[T].zero)
  }

  // create config.lanes filters and connect them up
  val filters = groupedWindow.zipWithIndex.map{ case (taps, index) => Module(new PFBLane(taps, cycleTime, config, index)) }
  filters.zip(in2).foreach( { case (filt, port) => filt.io.data_in := port; filt.io.valid := in.valid } )
  filters.zip(out_data).foreach( { case (filt, port) => port := filt.io.data_out } )
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
  * @param coeffs Seq of window coefficients
  * @param delay Size of the delays in the PFB. This also ends up being the number of parallel
  *              FIR filters.
  * @tparam T
  */
class PFBLane[T<:Data:Real](
  val coeffs: Seq[Double],
  val delay: Int,
  val config: PFBParams[T],
  val lane: Int

) extends Module {
  val io = IO(new Bundle {
    val data_in  = Input(config.genInInternal)
    val data_out = Output(config.genOutInternal)
    val valid = Input(Bool())
    val count = Input(UInt(log2Up(config.outputWindowSize / config.lanes).W))
  })

  require(coeffs.length % delay == 0)

  //println("This lane has coefficients:")
  //val temp = coeffs.map(x => x*math.pow(2,17))
  //println(temp.toArray.deep.mkString("\n"))

  // convert window coefficients to genTap type literals
  def convert[T <: Data:Real](x: Double, gen: T): T = {
    gen match { 
      case _: UInt => round(x).toInt.U.asInstanceOf[T]
      case _: SInt => round(x).toInt.S.asInstanceOf[T]
      case f: FixedPoint =>
        f.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            FixedPoint.fromDouble(x, gen.getWidth.W, binaryPoint.BP).asInstanceOf[T]
          case _ =>
            throw DspException(s"Error: convert: Can't create FixedPoint from signal template $f")
        }
    }
  }

  // pessimistically takes absolute value of literal
  var coeffsGrouped  = coeffs.grouped(delay).toSeq
  if (!config.transposed) { coeffsGrouped = coeffsGrouped.reverse }
  val tap_maxes      = Array.fill(coeffsGrouped.length)(BigInt(0))
  val coeffsWire     = coeffsGrouped.zipWithIndex.map{case (tapGroup, index) => {
    Vec(tapGroup.map(d => { 
        val y = convert(d, config.genTapInternal)
        //println(s"Coeff $d has lit value ${y.litValue}")
        //println(s"index = $index")
        tap_maxes(index) = tap_maxes(index).max(y.litValue.abs)
        y 
    }))
  }}


  // be smart about bitwidths, but conservative
  // multiply
  val mul_maxes = Array.fill(tap_maxes.length)(BigInt(0))
  val in_delays = Wire(Vec(config.outputWindowSize, config.genInInternal))
  in_delays.zipWithIndex.foreach{case (in, index) => {
    if (index == 0) { in := io.data_in }
    else { in := ShiftRegisterMem(in_delays(index-1), delay) }
  }}
  val products = coeffsWire.zipWithIndex.map{case (tap, index) => ShiftRegister( {
    // calculate worst-case values, assumes tap_max is positive and worst-case input is most negative value
    val tap_max = tap_maxes(index)
    val in_max = BigInt(pow(2, io.data_in.getWidth-1).toInt) // assumes signed
    val out_max = tap_max*in_max
    mul_maxes(index) = out_max
    val needed_bits = out_max.bitLength+1 // add sign bit
    //println(s"max_coeff = $tap_max")

    // input and tap for transposed vs. not
    val in = Wire(config.genInInternal)
    in := io.data_in
    val t = Wire(config.genTapInternal)
    t := tap(io.count)
    if (!config.transposed) {
      in := in_delays(index)
    }

    val resType = config.genInInternal match {
      case i: SInt => 
        SInt(needed_bits.W) 
      case u: UInt =>
        UInt(needed_bits.W)
      case f: FixedPoint => {
        val bp1 = f.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) => binaryPoint
          case _ => throw DspException(s"Error: convert: Can't create FixedPoint from signal template $f")
        }
        val bp2 = config.genTapInternal.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) => binaryPoint
          case _ => throw DspException(s"Error: convert: Can't create FixedPoint from signal template $f")
        }
        FixedPoint(needed_bits.W, (bp1+bp2).BP)
      }
    }
    val resWire = Wire(resType)
    resWire := DspContext.withTrimType(NoTrim) { t.context_*(in) }

    // check for overflow
    // delay assertion since poking happens after step
    val res = DspContext.withTrimType(NoTrim) { t.context_*(in) }
    //val assert_reg = RegNext(reset.toBool || res === resWire.asInstanceOf[T] || !io.valid, false.B)
    //assert(assert_reg, s"Error: overflow (multiply) on lane $lane, product $index")

    //val actual_bits = res.getWidth
    //println(s"mul output needs $needed_bits bits, but would get $actual_bits bits")

    //resWire.asInstanceOf[T]
    res
  }, config.multiplyPipelineDepth)}

  // be smart about bitwidths, but conservative
  // adder tree
  val valid_mul_delay = ShiftRegisterWithReset(io.valid, config.multiplyPipelineDepth, false.B)
  val valid_add_delay = Wire(Vec(products.length, Bool()))
  var index = 0
  var sum = BigInt(0)
  val result = products.reduceLeft { (prev:T, prod:T) =>
    if (index == 0) {
      sum = mul_maxes(0) + mul_maxes(1)
      index = index + 1
      valid_add_delay(0) := ShiftRegisterWithReset(valid_mul_delay, delay, false.B) // not used
      valid_add_delay(1) := ShiftRegisterWithReset(valid_mul_delay, delay, false.B)
    } else {
      sum = sum + mul_maxes(index)
      valid_add_delay(index) := ShiftRegisterWithReset(valid_add_delay(index-1), delay, false.B)
    }
    val needed_bits = sum.bitLength+1
    val shift = ShiftRegisterMem(prev, if (config.transposed) delay else 0)

    val resType = prev match {
      case i: SInt => 
        SInt(needed_bits.W) 
      case u: UInt =>
        UInt(needed_bits.W)
      case f: FixedPoint => {
        val bp1 = f.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) => binaryPoint
          case _ => throw DspException(s"Error: convert: Can't create FixedPoint from signal template $f")
        }
        val bp2 = prod.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) => binaryPoint
          case _ => throw DspException(s"Error: convert: Can't create FixedPoint from signal template $f")
        }
        FixedPoint(needed_bits.W, max(bp1,bp2).BP)
      }
    }

    val resWire = Wire(resType)
    resWire := DspContext.withOverflowType(Grow) { shift.context_+(prod) }

    // check for overflow
    // delay assertion since poking happens after step
    val res = DspContext.withOverflowType(Grow) { shift.context_+(prod) }
    val assert_reg = RegNext(reset.toBool || res === resWire.asInstanceOf[T] || !valid_add_delay(index), true.B)
    assert(assert_reg, s"Error: overflow (add) on lane $lane, adder $index")

    //val actual_bits = res.getWidth
    //println(s"add output needs $needed_bits bits, but would get $actual_bits bits")

    index = index + 1
    resWire.asInstanceOf[T]
  }
  val result_delay = ShiftRegister(result, config.outputPipelineDepth)
  val final_valid = ShiftRegisterWithReset(valid_add_delay(valid_add_delay.length-1), config.outputPipelineDepth, false.B)
  //println(s"result has bitwidth ${result_delay.getWidth}")
  val savings = io.data_in.getWidth + coeffsWire(0)(0).getWidth + products.length - 1 - result_delay.getWidth
  //println(s"saved $savings bits")

  //println(s"trying to fit into ${config.genOutInternal.getWidth} bits")
  val result_delay_uint = result_delay.asUInt
  val truncated_bits = result_delay.getWidth - io.data_out.getWidth
  val overflow = Wire(Bool())
  overflow := false.B
  if (truncated_bits > 0) {
    val overflow_bits = result_delay_uint.head(truncated_bits+1)
    overflow := ! ( (overflow_bits.andR) || (!overflow_bits.orR) )
  }

  // check for overflow
  // delay assertion since poking happens after step
  val assert_reg = RegNext(reset.toBool || !overflow || !final_valid, true.B)
  assert(assert_reg, s"Error: overflow (output) on lane $lane")

  // output results
  io.data_out := result_delay
}



case class PFBParams[T <: Data:Real]
(
  // generic
  genIn: T,
  genOut: Option[T] = None,
  name: String = "pfb",

  // custom
  windowFunc: WindowConfig => Seq[Double],
  numTaps: Int,
  outputWindowSize: Int,
  lanes: Int = 8,
  outputPipelineDepth: Int = 1,
  multiplyPipelineDepth: Int = 1,
  genTap: Option[T] = None,
  transposed: Boolean = false

) {
  requireIsChiselType(genIn,  s"genIn ($genIn) must be chisel type")
  genOut.foreach(g => requireIsChiselType(g, s"genOut ($g) must be chisel type"))
  genTap.foreach(g => requireIsChiselType(g, s"genTap ($g) must be chisel type"))

  val processingDelay = (numTaps-1)*(outputWindowSize/lanes)+outputPipelineDepth+multiplyPipelineDepth

  val genInInternal = genIn
  val genOutInternal = genOut.getOrElse(genIn)
  val genTapInternal = genTap.getOrElse(genIn)
  val inWidthBytes = (genInInternal.getWidth*lanes + 7)/ 8
  val outWidthBytes = (genOutInternal.getWidth*lanes + 7)/ 8
  val bytesAdded = outWidthBytes - inWidthBytes

  // scale coefficients array to fit into gen bits
  def scale[T <: Data:Real](coeffs: Seq[Double], gen: T): Seq[Double] = {
    val x = coeffs.reduceLeft(_ max _)
    val n = coeffs.reduceLeft(_ min _)
    require(x >= abs(n), "Error: absolute value of the minimum coefficient value must be less than or equal to the maximum coefficient value")
    val width = gen.getWidth
    val max_ref = pow(2, width-1)-1

    val s = gen match { 
      case f: FixedPoint =>
        f.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            pow(2, -binaryPoint)
          case _ =>
            throw DspException(s"Error: convert: Can't create FixedPoint from signal template $f")
        }
      case _ => 1.0
    }

    coeffs.map(y => y/x*max_ref*s)
  }


  // various checks for validity
  val window = scale(windowFunc(new WindowConfig(numTaps, outputWindowSize)), genTapInternal)
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
  */
case class WindowConfig(
                         numTaps: Int,
                         outputWindowSize: Int
)
