// See LICENSE for license details.

// Original author: Stevo Bailey (stevo.bailey@berkeley.edu)
// Port by: Paul Rigge (rigge@berkeley.edu)

package pfb

import chisel3._
import chisel3.util._
import dspjunctions.ValidWithSync
import dsptools._
import dsptools.numbers._
import dsptools.numbers.implicits._
import dspblocks._
import craft._

/**
  * IO Bundle for PFB
  * @param genIn Type generator for input to [[PFB]].
  * @param genOut Optional type generator for output, `genIn` if `None`.
  * @param lanes Number of lanes for both input and output
  * @tparam T
  */
class PFBIO[T <: Data](genIn: => T,
                       genOut: => Option[T] = None,
                       lanes: Int) extends Bundle {
  val data_in  = Input (ValidWithSync(Vec(lanes, genIn)))
  val data_out = Output(ValidWithSync(Vec(lanes, genOut.getOrElse(genIn))))

  val data_set_end_status = Output(Bool())
  val data_set_end_clear = Input(Bool())
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
class PFB[T<:Data:Ring](genIn: => T,
                        genOut: => Option[T] = None,
                        genTap: => Option[T] = None,
                        val convert: Double => T,
                        val config: PFBConfig = PFBConfig()
                        ) extends Module {
  val io = IO(new PFBIO(genIn, genOut, config.lanes))

  // split window up into config.lanes different sub-windows
  val groupedWindow = (0 until config.lanes).map(
    config.window.drop(_).grouped(config.lanes).map(_.head).toSeq
  )

  // resyncrhonize when valid goes high
  val valid_delay = Reg(next=io.data_in.valid)
  val cycleTime = config.outputWindowSize / config.lanes
  val counter = CounterWithReset(true.B, cycleTime, io.data_in.sync, ~valid_delay & io.data_in.valid)._1

  // user-defined latency, account for pipeline delays automatically
  val latency = config.processingDelay + config.multiplyPipelineDepth + config.outputPipelineDepth
  io.data_out.sync := ShiftRegisterWithReset(counter === (cycleTime-1).U, latency, 0.U) // don't let valid gate sync
  io.data_out.valid := ShiftRegisterWithReset(io.data_in.valid, latency, 0.U)

  // feed in zeros when invalid
  val in = Wire(Vec(config.lanes, genIn))
  when (io.data_in.valid) {
    in := io.data_in.bits
  } .otherwise {
    in := Vec.fill(config.lanes)(Ring[T].zero)
  }

  // data set end flag
  val valid_out_delay = Reg(next=io.data_out.valid)
  val dses = Reg(init=false.B)
  when (io.data_set_end_clear) {
    dses := false.B
  } .elsewhen (valid_out_delay & ~io.data_out.valid) {
    dses := true.B
  }
  io.data_set_end_status := dses

  // create config.lanes filters and connect them up
  val filters = groupedWindow.map( taps => Module(new PFBLane(genIn, genOut, genTap, taps, cycleTime, convert, config)))
  filters.zip(in).foreach( { case (filt, port) => filt.io.data_in := port } )
  filters.zip(io.data_out.bits).foreach( { case (filt, port) => port := ShiftRegister(filt.io.data_out, config.outputPipelineDepth) } )
  filters.foreach (f => {
    f.io.count := counter
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
class PFBLane[T<:Data:Ring](
  genIn: => T,
  genOut: => Option[T] = None,
  genTap: => Option[T] = None,
  val coeffs: Seq[Double],
  val delay: Int,
  val convert: Double => T,
  val config: PFBConfig = PFBConfig()

) extends Module {
  val io = IO(new Bundle {
    val data_in  = Input(genIn)
    val data_out = Output(genOut.getOrElse(genIn))
    val count = Input(UInt(log2Up(config.outputWindowSize / config.lanes).W))
  })

  require(coeffs.length % delay == 0)

  val coeffsGrouped  = coeffs.grouped(delay).toSeq
  val coeffsReversed = coeffsGrouped.map(_.reverse).reverse
  val coeffsWire     = coeffsReversed.map(tapGroup => {
    val coeffWire = Wire(Vec(tapGroup.length, genTap.getOrElse(genIn)))
    coeffWire.zip(tapGroup).foreach({case (t,d) => t := convert(d)})
    coeffWire
  })

  val products = coeffsWire.map(tap => DspContext.withTrimType(NoTrim) { tap(io.count) * io.data_in })

  val result = products.reduceLeft { (prev:T, prod:T) =>
    ShiftRegister(prod, config.multiplyPipelineDepth) + ShiftRegisterMem(prev, delay, name = this.name + "_sram")
  }

  io.data_out := result
}
