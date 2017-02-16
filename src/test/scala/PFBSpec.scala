// See LICENSE for license details.

package pfb

//scalastyle:off magic.number

import breeze.linalg._
import breeze.numerics.abs
import breeze.signal.fourierTr
import chisel3._
import chisel3.experimental._
import chisel3.iotesters.{PeekPokeTester, TesterOptionsManager}
import co.theasi.plotly._
import cde.Parameters
import dsptools.DspTester
import dspblocks._
import dsptools.numbers.DspReal
import dsptools.numbers.implicits._
import org.scalatest.{FlatSpec, Matchers, Tag}

object LocalTest extends Tag("edu.berkeley.tags.LocalTest")

/**
  * Basic tester that persists after calls to driver
  * The driver calls finish, which we override here to do nothing
  * You should remember to call actualFinish when you are done!
  * @param c
  * @param verbose
  * @tparam T
  */
class PFBTester[T<:Data](val c: PFB[T], verbose: Boolean = true) extends DspTester(c) {
  poke(c.io.data_in.valid, 1)
  poke(c.io.data_in.sync,  0)

  // this is a hack to use PFBTester outside of the normal driver methods
  override def finish = true

  def actualFinish = super.finish
}

class PFBStepTester[T <: Data](c: PFB[T], stepSize: Int, expectedOutput: Seq[Double], threshold: Double = 1.0e-5)
  extends DspTester(c) {
  poke(c.io.data_in.valid, 1)
  poke(c.io.data_in.sync,  0)

  val windowSize = c.config.windowSize
  val numTaps    = c.config.numTaps
  val parallelism = c.config.parallelism

  (0 until (expectedOutput.length / parallelism)) foreach (next => {
    val toPoke = if(next * parallelism < stepSize) 1.0 else 0.0
    c.io.data_in.bits.foreach { port => dspPoke(port, toPoke) }
    println(s"Poked $toPoke")
    val retval = c.io.data_out.bits.map { port => dspPeek(port).left.get }
    for (i <- 0 until parallelism) {
      val idx = next * parallelism + i
      println(s"Lane=$i\tRetval= ${retval(i)}\t Expected= ${expectedOutput(idx)}")
      assert(abs(retval(i) - expectedOutput(idx)) / (expectedOutput(idx) + threshold) < threshold,
       s"Output ${retval(i)} did not match expected value ${expectedOutput(idx)}")
    }
    step(1)
  })
}

class PFBLaneTester[T <: Data](c: PFBLane[T, Double],
                               val start: Int = -50,
                               val stop: Int  = 50,
                               val step: Int  = 1,
                               val threshold: Double = 0.1
                              ) extends DspTester(c) {
  def computedResult(n: Int): Double = {
    val delay = c.delay
    val nTaps = c.coeffs.length / c.delay
    val counterValue = (n - start) / step
    Seq.tabulate(nTaps){ x => {
      val samp = max(n - x * delay, 0)
      val tapidx = counterValue - x * nTaps
      val tap  = if (tapidx >= 0) c.coeffs(tapidx % c.coeffs.length) else 0
      tap * samp
    }}.sum
  }
  for (num <- start to stop by step) {
    dspPoke(c.io.data_in, num.toDouble)
    println(dspPeek(c.io.data_out).left.toString)
    println(s"Should be ${computedResult(num)} at num ${num}, is actually ${dspPeek(c.io.data_out).left.get}")
    //assert(math.abs(dspPeek(c.io.data_out).left.get - computedResult(num)) < threshold )
    step(1)
  }
}

object leakageTester {
  def testSignal[T<:Data](dut: PFBTester[T], signal: Seq[Double]): Seq[Double] = {
    dut.reset(5)
    val config = dut.c.config
    val io = dut.c.io
    val groupedSignal: Seq[Seq[Double]] = signal.grouped(config.parallelism).toSeq

    groupedSignal.flatMap(sigGroup => {
      sigGroup.zip(io.data_in.bits).foreach { case(sig, port) => dut.dspPoke(port, sig) }
      val retval = io.data_out.bits.map (x => dut.dspPeek(x).left.get)
      dut.step(1)
      retval
    })
  }

  def getEnergyAtBin(x_t: Seq[Double], bin: Int) : Double = {
    val fftSize = x_t.length
    val tform = fourierTr(DenseVector(x_t.toArray)).toArray.toSeq
    (tform(bin).abs / x_t.length) ** 2
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

  def setupTester[T <: Data](c: () => PFB[T]): PFBTester[T] = {
    var tester: PFBTester[T] = null
    chisel3.iotesters.Driver.execute(Array("-fimed", "2000"), c) (c => {
      val t = new PFBTester(c, verbose=false)
      tester = t
      t
    })

    tester
  }

  def teardownTester[T <: Data](tester: PFBTester[T]): Unit = {
    tester.actualFinish
  }

  def apply[T<:Data](c: () => PFB[T], config: PFBConfig, numBins: Int = 3, samples_per_bin: Int = 10): Unit = {
    val windowSize = config.windowSize
    val fftSize    = config.outputWindowSize
    val testBin    =  fftSize / 6
    val delta_fs = (-numBins.toDouble to numBins.toDouble by (1.0 / samples_per_bin)).toSeq
    val rawResults = delta_fs map {delta_f => {
      getEnergyAtBin(getTone(fftSize, (testBin + delta_f) / fftSize), testBin)
    }}
    val simResults = delta_fs map {delta_f => {
      val tone = getTone(windowSize, (testBin + delta_f) / fftSize)
      val simPfb = simWindow(tone, config)
      getEnergyAtBin(simPfb, testBin)
    }}
    val chiselResults = delta_fs.map({delta_f => {//delta_fs.par.map({delta_f => {
      val tester = setupTester(c)
      val tone = getTone(windowSize, (testBin + delta_f) / fftSize)
      val testResult = testSignal(tester, tone)
      val lastWindow = testResult.drop(testResult.length - fftSize)
      teardownTester(tester)
      getEnergyAtBin(lastWindow, testBin)
    }})//.seq
    val p = Plot()
      .withScatter(delta_fs, chiselResults,    ScatterOptions().name("Chisel"))
      .withScatter(delta_fs, rawResults, ScatterOptions().name("No window"))
      .withScatter(delta_fs, simResults, ScatterOptions().name("Sim window"))
    draw(p, "leakage-parallel", writer.FileOptions(overwrite=true))
  }
}

class PFBSpec extends FlatSpec with Matchers {
  import chisel3.{Bool, Bundle, Module, Mux, UInt, Vec}
  behavior of "PFB"
  it should "build with SInt" in {
    //implicit val p: Parameters = Parameters.root(new DspConfig().toInstance)
    chisel3.iotesters.Driver(() => new PFB(SInt(10.W), Some(SInt(16.W)),
      config = PFBConfig(
        outputWindowSize = 4, numTaps = 4, parallelism = 2
      ))) {
      c => new PFBTester(c)
    } should be(true)
  }

  it should "build with FixedPoint" in {
    chisel3.iotesters.Driver(() => new PFB(FixedPoint(10.W, 5.BP), Some(FixedPoint(16.W, 7.BP)),
      config = PFBConfig(
        outputWindowSize = 4, numTaps = 4, parallelism = 2
      ))) {
      c => new PFBTester(c)
    } should be(true)
  }

  it should "build any parameterization" in {
    val numTaps = Seq(1, 7, 16, 43)
    val outputWindowSize = Seq(4, 256, 1024)
    val parallelism = Seq(2, 8)
    for (i <- numTaps) {
      for (j <- outputWindowSize) {
        for (k <- parallelism) {
          if (k <= j) {
            chisel3.iotesters.Driver(() => new PFB(FixedPoint(4.W, 2.BP), Some(FixedPoint(2.W, 7.BP)),
              config = PFBConfig(
                outputWindowSize = j, numTaps = i, parallelism = k
              ))) {
              c => new PFBTester(c)
            } should be(true)
          }
        }
      }
    }
  }

  ignore should "have the correct step response" in {
    {
      val config = PFBConfig(
        windowFunc = WindowConfig => Seq(1.0, 2.0, 3.0, 4.0),
        numTaps = 2,
        outputWindowSize = 2,
        parallelism = 2
      )
      val expected = Seq(1.0, 2.0, 3.0, 4.0, 0.0, 0.0, 0.0, 0.0) //Seq(3.0, 4.0, 1.0, 2.0, 0.0, 0.0, 0.0, 0.0)
      val stepSize = config.windowSize / (config.numTaps * config.parallelism)

      chisel3.iotesters.Driver(() => new PFB(SInt(10.W), config = config)) {
        c => new PFBStepTester(c, stepSize, expected)
      } should be(true)
    }
    /*{
      val config = PFBConfig(
        windowFunc = WindowConfig => Seq(1.0, 2.0, 3.0, 4.0),
        numTaps = 2,
        outputWindowSize = 2,
        parallelism = 1
      )
      val expected = Seq(1.0, 2.0, 3.0, 4.0, 0.0, 0.0, 0.0, 0.0)
      val stepSize = config.windowSize / (config.numTaps * config.parallelism)
      chisel3.iotesters.Driver(() => new PFB(SInt.width(10), config = config)) {
        c => new PFBStepTester(c, stepSize, expected)
      } should be(true)
    }*/
  }

  it should "reduce leakage" taggedAs(LocalTest) in {
    val config = PFBConfig(
      windowFunc = blackmanHarris.apply,
      numTaps = 8,
      outputWindowSize = 256,
      parallelism=16
    )
    leakageTester( () => new PFB(FixedPoint(32.W, 16.BP), config=config), config, numBins = 5, samples_per_bin = 5 )
  }

  behavior of "PFBLane"
  it should "build and run correctly" in {
    chisel3.iotesters.Driver(() => new PFBLane[SInt,Double](
      SInt(8.W), Some(SInt(10.W)), Some(SInt(10.W)),
        Seq(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), 4)
    ) {
      c => new PFBLaneTester(c)
    } should be (true)
  }

  behavior of "SInt"
  ignore should "allow me to assign to smaller widths" in {
    chisel3.iotesters.Driver.execute( Array("-tiv"), () => new SIntPassthrough()) {
      c => new SIntPassthroughTester(c)
    } should be (true)
  }
}

class SIntPassthrough extends Module {
  val io = IO(new Bundle {
    val in = Input(SInt.width(5))
    val out = Output(SInt.width(5))
  })


  val times4 = Reg(t = SInt(8.W), next=io.in +& io.in +& io.in +& io.in)

  io.out := times4
}

class SIntPassthroughTester(c: SIntPassthrough) extends PeekPokeTester(c) {
  poke(c.io.in, 15)
  step(1)
  expect(c.io.out, -4)
  println(peek(c.io.out).toString)
}
