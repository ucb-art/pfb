// See LICENSE for license details.

package pfb

//scalastyle:off magic.number

import breeze.linalg._
import breeze.signal.fourierTr
import chisel3._
import chisel3.iotesters.PeekPokeTester
import co.theasi.plotly._
import dsptools.DspTester
import dsptools.numbers.DspReal
import dsptools.numbers.implicits._
import org.scalatest.{FlatSpec, Matchers}

class PFBTester[T<:Data](c: PFB[T]) extends DspTester(c) {
  //poke(c.io.sync_in, 0)

  for(num <- -50 to 50) {
    c.io.data_in.foreach { port => dspPoke(port, num.toDouble) }
    step(1)
    c.io.data_out.foreach { port => println(dspPeek(port).toString)}
  }
}

class PFBConstantInput[T<:Data](c: PFB[T], verbose: Boolean = true) extends DspTester(c, verbose=verbose) {
  val windowSize = c.config.windowSize
  val numTaps    = c.config.numTaps
  val parallelism = c.config.parallelism

  val result = (0 until windowSize / parallelism * 2).foldLeft(Seq[Double]()) ( (sum:Seq[Double], next: Int) => {
    val toPoke = if(next < windowSize / (numTaps * parallelism)) 1.0 else 0.0
    c.io.data_in.foreach { port => dspPoke(port, toPoke)}
    val retval = c.io.data_out.map { port => dspPeek(port).left.get }
    step(1)
    sum ++ retval
  })

  println()
  result.foreach { x => print(x.toString + ", ")}
  println()

  result.zip(c.config.window).map({case(x,y) => assert(x == y)})
}

class PFBFilterTester[T<:Data](c: PFBFilter[T,Double],
                               val start: Int = -50,
                               val stop: Int  = 50,
                               val step: Int  = 1,
                               verbose: Boolean = true
                              ) extends DspTester(c, verbose=verbose) {
  def computedResult(n: Int): Double = {
    val delay = c.taps.length
    val nTaps = c.taps(0).length
    val counterValue = (n - start) / step
    val taps  = c.taps(counterValue % c.taps.length)
    if (verbose) println(s"Using taps ${taps}")
    val samples = Seq.tabulate(nTaps){x => {
      val samp = n - x * delay
      if (samp >= start) samp
      else 0
    }}
    taps.zip(samples).map { case(x,y) => x*y }  reduce (_+_)
  }
  for (num <- start to stop by step) {
    dspPoke(c.io.data_in, num.toDouble)
    if (verbose) {
      println(dspPeek(c.io.data_out).toString)
      println(s"Should be ${computedResult(num)} at num ${num}, is actually ${dspPeek(c.io.data_out).left.get}")
    }
    assert(math.abs(dspPeek(c.io.data_out).left.get - computedResult(num)) < 0.1 )
    step(1)
  }
}

object leakageTester {
  def testSignal[T<:Data](c: () => PFB[T], signal: Seq[Double], verbose: Boolean = true): Seq[Double] = {
    var getOutput: () => Seq[Double] = null
    chisel3.iotesters.Driver(c) {
      c => {
        val tester = new PFBSignalTester(c, signal)
        // this is *so* shady
        getOutput = () => tester.output
        tester
      }
    }
    getOutput()
  }

  def getEnergyAtBin(x_t: Seq[Double], bin: Int) : Double = {
    val fftSize = x_t.length
    //println(s"fftSize = $fftSize, bin = $bin")
    val tform = fourierTr(DenseVector(x_t.toArray)).toArray.toSeq
    //println(s"fft = ${tform.map(_.abs ** 2)}")
    tform.map(_.abs ** 2).apply(bin)
    //tform(bin).abs ** 2
  }

  def getTone(numSamples: Int, f: Double): Seq[Double] = {
    (0 until numSamples).map(i => math.sin(2 * math.Pi * f * i))
  }

  def simWindow(signal: Seq[Double], config: PFBConfig): Seq[Double] = {
    assert(signal.length == config.windowSize)
    val signalByWindow = signal.zip(config.window).map({case(x,y)=>x*y})
    (0 until config.outputWindowSize).map(i => {
      (0 until config.numTaps).foldLeft(0.0) { case(sum, j) => {
        sum + signalByWindow(i + j * config.outputWindowSize)
      }}
    })
  }

  def apply[T<:Data](c: () => PFB[T], config: PFBConfig, numBins: Int = 3, os: Int = 10): Unit = {
    val windowSize = config.windowSize
    val fftSize    = config.outputWindowSize
    val testBin    = fftSize / 6
    val delta_fs = (-numBins.toDouble to numBins.toDouble by (1.0 / os)).toSeq
    val rawResults = delta_fs map {delta_f => {
      getEnergyAtBin(getTone(fftSize, (testBin + delta_f) / fftSize), testBin)
    }}
    val simResults = delta_fs map {delta_f => {
      val tone = getTone(windowSize, (testBin + delta_f) / fftSize)
      val simPfb = simWindow(tone, config)
      getEnergyAtBin(simPfb, testBin)
    }}
    val chiselResults = delta_fs map {delta_f => {
      println(s"delta_f = $delta_f")
      val tone = getTone(1 * windowSize + fftSize, (testBin + delta_f) / fftSize)
      val testResult = testSignal(c, tone, verbose=false)
      println(s"Sim test result = ${simWindow(getTone(windowSize, (testBin + delta_f)/ fftSize), config)}")
      println(s"Test result = $testResult")
//      val fftResult = fourierTr(DenseVector(testResult.drop(testResult.length - fftSize).toArray)).toArray.map(_.abs**2)
//      val p = Plot().withScatter((0 until fftSize), fftResult)
//      draw(p, s"FFT for delta_f = $delta_f", writer.FileOptions(overwrite=true))
      getEnergyAtBin(testResult.drop(testResult.length - fftSize), testBin)
    }}
    println(s"Chisel: $chiselResults")
    println(s"Simulated: $simResults")
    println(s"No window: $rawResults")
    val p = Plot()
      .withScatter(delta_fs, chiselResults,    ScatterOptions().name("Chisel"))
      .withScatter(delta_fs, rawResults, ScatterOptions().name("No window"))
//      .withScatter(x, simresults_normalized, ScatterOptions().name("Sim window"))
    draw(p, "leakage", writer.FileOptions(overwrite=true))
  }
}


class PFBSignalTester[T<:Data](c: PFB[T], signal: Seq[Double], verbose: Boolean = true) extends DspTester(c, verbose=verbose) {
  val grouped_signal = signal.grouped(c.config.parallelism).toList
  val output: Seq[Double] = grouped_signal.flatMap(sig=> {
    sig.zip(c.io.data_in).foreach({case(value, port) => {
      dspPoke(port, value)
    } })
    val toret = c.io.data_out.map(port=>dspPeek(port).left.get)
    if(peek(c.io.sync) != 0) {
      println(s"Sync at ${grouped_signal.indexOf(sig)}")
    }
    step(1)
    toret
  })
}

class PFBSpec extends FlatSpec with Matchers {
  import chisel3.{Bool, Bundle, Module, Mux, UInt, Vec}
  behavior of "Vecs"
  ignore should "don't work nicely with underlying things that have different widths" in {
    class VecTest extends Module {
      val io = Input(new Bundle {
        val in = Input(Bool())
        val out = Output(UInt(width=16))
      })
      val c = Mux(io.in,
//        Vec(UInt(1), UInt(2), UInt(3)), // Fail
        Vec(UInt(1, width=5), UInt(2), UInt(3)), // Pass
        Vec(UInt(10), UInt(20), UInt(30)))
      io.out := c(0)
    }
    class VecTestTester(c: VecTest) extends PeekPokeTester(c) {
      poke(c.io.in, 0)
      step(1)
      expect(c.io.out, 10)
      poke(c.io.in, 1)
      step(1)
      expect(c.io.out, 1)
    }
    println(Driver.emit( () => new VecTest()) )
    chisel3.iotesters.Driver(() =>
      new VecTest) { c => new VecTestTester(c) } should be (true)
  }
  behavior of "PFB"
  ignore should "build with SInt" in {
    chisel3.iotesters.Driver(() => new PFB(SInt(width = 10), Some(SInt(width = 16)),
      config = PFBConfig(
        outputWindowSize = 4, numTaps = 4, parallelism = 2
      ))) {
      c => new PFBTester(c)
    } should be(true)
  }

  ignore should "have the correct step response" in {
    chisel3.iotesters.Driver(() => new PFB(DspReal(1.0),
      config=PFBConfig(
        windowFunc = blackmanHarris.apply,
        numTaps = 8,
        outputWindowSize = 128,
        parallelism=2
      ))) {
      c => new PFBConstantInput(c, verbose=true)
    } should be (true)
  }

  it should "reduce leakage" in {
    val config = PFBConfig(
      windowFunc = blackmanHarris.apply,
      numTaps = 4,
      outputWindowSize = 128,
      parallelism=2
    )
    leakageTester( () => new PFB(DspReal(0.0), config=config), config )
  }

  behavior of "PFBFilter"
  ignore should "build and run" in {
    chisel3.iotesters.Driver(() => new PFBFilter[SInt,Double](
      SInt(width=8), Some(SInt(width=10)), Some(SInt(width=10)),
        Seq(Seq(1.0,2.0), Seq(3.0,4.0), Seq(5.0,6.0), Seq(7.0,8.0)))
    ) {
      c => new PFBFilterTester(c, verbose=false)
    } should be (true)
  }
}
