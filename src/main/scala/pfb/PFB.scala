// See LICENSE for license details.

// Original author: Stevo Bailey (stevo.bailey@berkeley.edu)
// Port by: Paul Rigge (rigge@berkeley.edu)

package pfb

import chisel3._
import chisel3.util.{Counter, ShiftRegister, log2Up}
import dsptools.numbers.Real
import dsptools.numbers.implicits._
import spire.algebra.Ring
import spire.math.{ConvertableFrom, ConvertableTo}

// polyphase filter bank io
class PFBIO[T<:Data](genIn: => T, genOut: => Option[T] = None,
                     windowSize: Int, parallelism: Int) extends Bundle {
  val data_in = Input(Vec(parallelism, genIn))
  val data_out = Output(Vec(parallelism, genOut.getOrElse(genIn)))
  //val sync_in = Input(UInt(log2Up(windowSize/parallelism)))
  val sync = Output(Bool())
  val overflow = Output(Bool())
}

object blackmanHarris {
  private val a0 = 0.35875
  private val a1 = 0.48829
  private val a2 = 0.14128
  private val a3 = 0.01168
  def apply(N: Int): Seq[Double] = Seq.tabulate(N) (i => {
    a0 -
      a1 * math.cos(2 * math.Pi * i.toDouble / (N - 1)) +
      a2 * math.cos(4 * math.Pi * i.toDouble / (N - 1)) -
      a3 * math.cos(6 * math.Pi * i.toDouble / (N - 1))
  })
  def apply(w: WindowConfig): Seq[Double] = blackmanHarris(w.outputWindowSize * w.numTaps)
}

object sincHamming {
  def apply(size: Int, nfft: Int): Seq[Double] = Seq.tabulate(size) (i=>{
    val term1 = 0.54 - 0.46 * breeze.numerics.cos(2 * scala.math.Pi * i.toDouble / size)
    val term2 = breeze.numerics.sinc(size.toDouble / nfft - 0.5 * (size.toDouble / nfft) )
    term1 * term2
  })
  def apply(w: WindowConfig): Seq[Double] = sincHamming(w.outputWindowSize * w.numTaps, w.outputWindowSize)
}

class PFBFilter[T<:Data:Ring:ConvertableTo, V:ConvertableFrom](
                 genIn: => T,
                 genOut: => Option[T] = None,
                 genTap: => Option[T] = None,
                 val taps: Seq[Seq[V]]
                 //conv: V=>T
               ) extends Module {
  val io = IO(new Bundle {
    val data_in = Input(genIn)
    val data_out = Output(genOut.getOrElse(genIn))
    val overflow = Output(Bool())
  })

  val delay = taps.length
  val count = Counter(taps.length)
  count.inc()
  val countDelayed = Reg(next=count.value)

  val tapsTransposed = taps.map(_.reverse).reverse.transpose.map( tap => {
    val tapsWire = Wire(Vec(tap.length, genTap.getOrElse(genIn)))
    tapsWire.zip(tap.reverse).foreach({case (t,d) => t := ConvertableTo[T].fromType(d)})
    tapsWire
  })

  val products = tapsTransposed.map(tap => tap(count.value) * io.data_in)

  val result = products.reduceLeft { (prev:T, prod:T) => prod + ShiftRegisterMem(delay, prev, init = Some(Ring[T].zero)) }

  io.data_out := result
}

case class WindowConfig(
                       numTaps: Int,
                       outputWindowSize: Int
                       )
case class PFBConfig(
                      windowFunc: WindowConfig => Seq[Double] = sincHamming.apply,
                      numTaps: Int = 4,
                      outputWindowSize: Int = 16,
                      parallelism: Int = 8,
                    // currently ignored
                      pipelineDepth: Int = 4,
                      useSinglePortMem: Boolean = false,
                      symmetricCoeffs: Boolean  = false,
                      useDeltaCompression: Boolean = false
                    ) {
  val window = windowFunc( WindowConfig(numTaps, outputWindowSize))
  val windowSize = window.length

  // various checks for validity
  assert(numTaps > 0, "Must have more than zero taps")
  assert(outputWindowSize > 0, "Output window must have size > 0")
  assert(outputWindowSize % parallelism == 0, "Number of parallel inputs must divide the output window size")
  assert(windowSize > 0, "PFB window must have > 0 elements")
  assert(windowSize == numTaps * outputWindowSize, "windowFunc must return a Seq() of the right size")
}

class PFB[T<:Data:Real](
                            genIn: => T,
                            genOut: => Option[T] = None,
                            genTap: => Option[T] = None,
                            val config: PFBConfig = PFBConfig()
                          ) extends Module {
  val io = IO(new PFBIO(genIn, genOut, config.windowSize, config.parallelism))

  val groupedWindow = (0 until config.parallelism).map( lane => {
    (0 until config.outputWindowSize / config.parallelism).map( group => {
      (group * config.parallelism + lane until config.windowSize by config.outputWindowSize).map(config.window(_))
    })
  })

  val cycleTime = config.outputWindowSize / config.parallelism
  val counter = Counter(cycleTime)
  counter.inc()

  io.sync := counter.value === UInt(cycleTime - 1)

  val filters = groupedWindow.map( taps => Module(new PFBFilter(genIn, genOut, genTap, taps)))
  filters.zip(io.data_in).foreach( { case (filt, port) => filt.io.data_in := port } )
  filters.zip(io.data_out).foreach( { case (filt, port) => port := filt.io.data_out } )
}

