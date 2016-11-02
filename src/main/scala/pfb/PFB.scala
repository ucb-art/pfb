// See LICENSE for license details.

// Original author: Stevo Bailey (stevo.bailey@berkeley.edu)
// Port by: Paul Rigge (rigge@berkeley.edu)

package pfb

import chisel3._
import chisel3.util.Counter
import dsptools.numbers.Real
import dsptools.numbers.implicits._
import spire.algebra.Ring
import spire.math.{ConvertableFrom, ConvertableTo}

/**
  * Case class for holding PFB configuration information
  * @param windowFunc A function that generates a window given window a `WindowConfig`,
  *                   which includes output window size and and number of taps.
  *                   Must give a window of size `numTaps * outputWindowSize`.
  * @param numTaps Number of taps, used when calling windowFunc. Must be > 0.
  * @param outputWindowSize Size of the output window, often the same as the size of an FFT following the PFB.
  *                         Must be > 0 and a multiple of `parallelism`.
  * @param parallelism Number of parallel lanes in the FFT.
  * @param pipelineDepth Not currently used
  * @param useSinglePortMem Not currently used
  * @param symmetricCoeffs Not currently used
  * @param useDeltaCompression Not currently used
  */
case class PFBConfig(
                      windowFunc: WindowConfig => Seq[Double] = sincHamming.apply,
                      numTaps: Int = 4,
                      outputWindowSize: Int = 16,
                      parallelism: Int = 8,
                    // the below are currently ignored
                      pipelineDepth: Int = 4,
                      useSinglePortMem: Boolean = false,
                      symmetricCoeffs: Boolean  = false,
                      useDeltaCompression: Boolean = false
                    ) {
  val window = windowFunc( WindowConfig(numTaps, outputWindowSize))
  val windowSize = window.length

  // various checks for validity
  require(numTaps > 0, "Must have more than zero taps")
  require(outputWindowSize > 0, "Output window must have size > 0")
  require(outputWindowSize % parallelism == 0, "Number of parallel inputs must divide the output window size")
  require(windowSize > 0, "PFB window must have > 0 elements")
  require(windowSize == numTaps * outputWindowSize, "windowFunc must return a Seq() of the right size")
}

/**
  * IO Bundle for PFB
  * @param genIn Type generator for input to PFB.
  * @param genOut Optional type generator for output, genIn if None.
  * @param parallelism Number of lanes for both input and output
  * @tparam T
  */
class PFBIO[T <: Data](genIn: => T,
                       genOut: => Option[T] = None,
                       parallelism: Int) extends Bundle {
  val data_in = Input(Vec(parallelism, genIn))
  val data_out = Output(Vec(parallelism, genOut.getOrElse(genIn)))
  val sync_in  = Input(Bool())
  val sync_out = Output(Bool())
  val overflow = Output(Bool())
}

/**
  *
  * @param genIn Type generator for input.
  * @param genOut Optional type generator for output, `genIn` by default.
  * @param genTap Optional type generator for window coefficients, `genIn` by default.
  * @param config PFB configuration object, includes the window function.
  * @tparam T
  */
class PFB[T<:Data:Real](genIn: => T,
                        genOut: => Option[T] = None,
                        genTap: => Option[T] = None,
                        val config: PFBConfig = PFBConfig()
                        ) extends Module {
  val io = IO(new PFBIO(genIn, genOut, config.parallelism))

  // split window up into config.parallelism different sub-windows
  val groupedWindow = (0 until config.parallelism).map(
    config.window.drop(_).grouped(config.parallelism).map(_.head).toSeq
  )

  val cycleTime = config.outputWindowSize / config.parallelism
  val counter = Counter(cycleTime)
  counter.inc()

  io.sync_out := counter.value === UInt(0)

  val filters = groupedWindow.map( taps => Module(new PFBLane(genIn, genOut, genTap, taps, cycleTime)))
  filters.zip(io.data_in).foreach( { case (filt, port) => filt.io.data_in := port } )
  filters.zip(io.data_out).foreach( { case (filt, port) => port := filt.io.data_out } )
}

/**
  * A single lane of the PFB. A full PFB will include >= 1 lanes operating in parallel.
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
class PFBLane[T<:Data:Ring:ConvertableTo, V:ConvertableFrom](
  genIn: => T,
  genOut: => Option[T] = None,
  genTap: => Option[T] = None,
  val coeffs: Seq[V],
  val delay: Int
) extends Module {
  val io = IO(new Bundle {
    val data_in = Input(genIn)
    val data_out = Output(genOut.getOrElse(genIn))
    val overflow = Output(Bool())
  })

  require(coeffs.length % delay == 0)

  val count = Counter(delay)
  count.inc()
  val countDelayed = Reg(next=count.value)

  val coeffsGrouped  = coeffs.grouped(delay).toSeq
  val coeffsReversed = coeffsGrouped.map(_.reverse).reverse
  val coeffsWire     = coeffsReversed.map(tapGroup => {
    val coeffWire = Wire(Vec(tapGroup.length, genTap.getOrElse(genIn)))
    coeffWire.zip(tapGroup).foreach({case (t,d) => t := ConvertableTo[T].fromType(d)})
    coeffWire
  })

  val products = coeffsWire.map(tap => tap(count.value) * io.data_in)

  val result = products.reduceLeft { (prev:T, prod:T) =>
    prod + ShiftRegisterMem(delay, prev, init = Some(Ring[T].zero))
  }

  io.data_out := result
}
