// See LICENSE for license details.

// Original author: Stevo Bailey (stevo.bailey@berkeley.edu)
// Port by: Paul Rigge (rigge@berkeley.edu)

package pfb

import chisel3._
import chisel3.util.Counter
import dspjunctions.ValidWithSync
import dsptools.numbers.Real
import dsptools.numbers.implicits._
import spire.algebra.Ring
import spire.math.{ConvertableFrom, ConvertableTo}
import dsptools.counters._

/**
  * IO Bundle for PFB
  * @param genIn Type generator for input to [[PFB]].
  * @param genOut Optional type generator for output, `genIn` if `None`.
  * @param parallelism Number of lanes for both input and output
  * @tparam T
  */
class PFBIO[T <: Data](genIn: => T,
                       genOut: => Option[T] = None,
                       parallelism: Int) extends Bundle {
  val data_in  = Input (ValidWithSync(Vec(parallelism, genIn)))
  val data_out = Output(ValidWithSync(Vec(parallelism, genOut.getOrElse(genIn))))
}

/**
  * Polyphase filter bank implementation. See
  * [[https://casper.berkeley.edu/wiki/The_Polyphase_Filter_Bank_Technique Casper]] for more details.
  * @param genIn Type generator for input.
  * @param genOut Optional type generator for output, `genIn` if `None`.
  * @param genTap Optional type generator for window coefficients, `genIn` if `None`.
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
  when (io.data_in.sync) {
    if (cycleTime > 1) counter.value := 0.U //counter.restart()
  }

  io.data_out.sync := counter.value === 0.U
  io.data_out.valid := ShiftRegisterWithReset(io.data_in.valid, cycleTime*config.numTaps, 0.U)

  // feed in zeros when invalid
  val in = Wire(Vec(config.parallelism, genIn))
  when (io.data_in.valid) {
    in := io.data_in.bits
  } .otherwise {
    in := Wire(Vec(config.parallelism, implicitly[Real[T]].zero))
  }

  val filters = groupedWindow.map( taps => Module(new PFBLane(genIn, genOut, genTap, taps, cycleTime)))
  filters.zip(in).foreach( { case (filt, port) => filt.io.data_in := port } )
  filters.zip(io.data_out.bits).foreach( { case (filt, port) => port := filt.io.data_out } )
  filters.foreach (f => {
    f.io.valid_in := io.data_in.valid
    f.io.sync_in := io.data_in.sync
  })
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
class PFBLane[T<:Data:Ring:ConvertableTo, V:ConvertableFrom](
  genIn: => T,
  genOut: => Option[T] = None,
  genTap: => Option[T] = None,
  val coeffs: Seq[V],
  val delay: Int
) extends Module {
  val io = IO(new Bundle {
    val data_in  = Input(genIn)
    val valid_in = Input(Bool())
    val sync_in  = Input(Bool())
    val data_out = Output(genOut.getOrElse(genIn))
  })

  require(coeffs.length % delay == 0)

  val en = io.valid_in
  val count = Counter(delay)

  when (en) { count.inc() }
  when (io.sync_in) {
    count.value := 0.U
  }

  val coeffsGrouped  = coeffs.grouped(delay).toSeq
  val coeffsReversed = coeffsGrouped.map(_.reverse).reverse
  val coeffsWire     = coeffsReversed.map(tapGroup => {
    val coeffWire = Wire(Vec(tapGroup.length, genTap.getOrElse(genIn)))
    coeffWire.zip(tapGroup).foreach({case (t,d) => t := ConvertableTo[T].fromType(d)})
    coeffWire
  })

  val products = coeffsWire.map(tap => tap(count.value) * io.data_in)

  val result = products.reduceLeft { (prev:T, prod:T) =>
    prod + ShiftRegisterMem(delay, prev, en = en, init = Some(Ring[T].zero))
  }

  io.data_out := result
}
