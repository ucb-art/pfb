package pfb

import chisel3._
import chisel3.core.FixedPoint
import chisel3.internal.firrtl.Width
import chisel3.iotesters._
import chisel3.util._
import dspblocks._
import dsptools.numbers.{Ring, DspReal}
//import dsptools.numbers.implicits._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.coreplex.BaseCoreplexConfig
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import org.scalatest.{FlatSpec, Matchers}
import scala.collection.{Seq, mutable}

import breeze.linalg._
import breeze.signal.fourierTr
import breeze.math.Complex
import chisel3.internal.firrtl.KnownBinaryPoint
import dsptools.DspTesterUtilities._
import dsptools.{DspTester, DspException}
import scala.math.{abs, pow, max, log10}




class PFBSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.root((new BaseCoreplexConfig).toInstance)

  def runTest[T <: Data : Ring](params: PFBParams[T], 
                                inWidth: Int,
                                in: Seq[BigInt]): Seq[BigInt] = {
    val out = new mutable.ArrayBuffer[BigInt]()
    val blindNodes = DspBlockBlindNodes.apply(
      AXI4StreamBundleParameters(
        n = inWidth,
        i = 1,
        d = 1,
        u = 1,
        hasData = true,
        hasStrb = false,
        hasKeep = false
      ),
      () => AXI4MasterNode(Seq(AXI4MasterPortParameters(Seq(AXI4MasterParameters("pfb")))))
    )

    val dut = () => LazyModule(DspBlock.blindWrapper(() => new PFB(params), blindNodes)).module

    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-twffn", "out.vcd", "-fiwv"), dut) {
      c => new PFBDataTester(c, in, out)
    }

    out
  }




  //////////////////////////////////////////
  /////// BASIC COMIPLATION TESTS //////////
  //////////////////////////////////////////

  def paramTest[T <: Data : Ring](
    genIn: T = SInt(32.W), 
    winFunc: WindowConfig => Seq[Double] = blackmanHarris.apply,
    winFuncName: String = "Blackman Harris",
    lanes: Int = 4,
    numTaps: Int = 4,
    outputWindowSize: Int = 128,
    convert: Double => T = (x: Double) => x.toInt.S
  ): Unit = {

    it should s"compile with the following parameters:\n" + 
      s"\t type ${genIn} and width ${genIn.getWidth}\n" +
      s"\t window function $winFuncName\n" +
      s"\t ${lanes} lanes, ${numTaps} taps, and ${outputWindowSize} window size" in {
      // setup PFB
      val params = PFBParams(
        genIn = genIn,
        numTaps = numTaps,
        outputWindowSize = outputWindowSize,
        windowFunc = winFunc,
        lanes = lanes,
        convert = convert 
      )

      val inWidthBytes = (params.genIn.getWidth*params.lanes + 7)/ 8

      // setup fake test data and run test
      val values = CustomFunctions.packInputStream(Seq.fill(10)(Seq.fill(params.lanes)(0.0)), params.genIn)
      val out = runTest(params, inWidthBytes, values)
    }
  }

  paramTest(genIn = UInt(32.W), convert = (x: Double) => x.toInt.U)
  paramTest(genIn = SInt(32.W), convert = (x: Double) => x.toInt.S)
  // doesn't work yet
  //paramTest(genIn = DspComplex(SInt(32.W), SInt(32.W)))
  paramTest(genIn = FixedPoint(32.W, 16.BP), convert = (x: Double) => FixedPoint.fromDouble(x, 32.W, 16.BP))
  // downsize coefficients to fit in 16 bits (32 BP)
  paramTest(genIn = FixedPoint(16.W, 32.BP), convert = (x: Double) => FixedPoint.fromDouble(x/math.pow(2, 32), 16.W, 32.BP))
  paramTest(winFunc = sincHanning.apply, winFuncName = "Sinc + Hanning")
  paramTest(lanes = 32)
  paramTest(lanes = 10, outputWindowSize = 200)
  paramTest(numTaps = 2)
  paramTest(numTaps = 12)
  paramTest(numTaps = 16)


  //////////////////////////////////////////
  /////////// COEFFICIENT TESTS ////////////
  //////////////////////////////////////////

  // return coefficient with some known delay when an impulse (single 1) is applied to the data input
  // do this for all coefficients

  def coeffTest(): Unit = {

    it should s"return the coefficients with all data as 1" in {

      // setup params
      val params = PFBParams(
        genIn = FixedPoint(20.W, 16.BP),
        numTaps = 6,
        outputWindowSize = 256,
        windowFunc = sincHanning.apply,
        lanes = 16,
        convert = (x: Double) => FixedPoint.fromDouble(x, 20.W, 16.BP),
        outputPipelineDepth = 0,
        multiplyPipelineDepth = 0
      )

      // setup blind nodes 
      val inWidthBytes = (params.genIn.getWidth*params.lanes + 7)/ 8

      // check each tap
      var success = true
      for (tap <- 0 until params.numTaps) {

        // setup test data
        val values = Array.fill(params.processingDelay+params.outputWindowSize/params.lanes)(Array.fill(params.lanes)(0.0))
        for (i <- tap*params.outputWindowSize until (tap+1)*params.outputWindowSize) {
          values(i/params.lanes)(i%params.lanes) = 1.0
        }
        //println(s"input = ${values.map(_.toSeq).toSeq}")
        val out = runTest(params, inWidthBytes, CustomFunctions.packInputStream(values.map(_.toSeq).toSeq, params.genIn))
        val unpackedOut = CustomFunctions.unpackOutputStream(params.genOut.getOrElse(params.genIn), params.lanes, out)
        //println(s"output = $unpackedOut")
        //println(s"window = ${params.window}")
        assert(unpackedOut.length == params.outputWindowSize)
        params.window.drop(tap*params.outputWindowSize).zip(unpackedOut).zipWithIndex.foreach{ case ((expected, actual), j) => {
          // check if actual is within quantization of data = 2^-BP
          //println(s"Checking if $actual == $expected")
          if (actual > expected+pow(2,-16) || actual < expected-pow(2,-16)) {
            println(s"Error, mismatch on coefficient $j, expected $expected, got $actual")
            success = false
          }
        }}
      }

      // check summation
      val values = Array.fill(params.processingDelay+params.outputWindowSize/params.lanes)(Array.fill(params.lanes)(1.0))
      //println(s"input = ${values.map(_.toSeq).toSeq}")
      val out = runTest(params, inWidthBytes, CustomFunctions.packInputStream(values.map(_.toSeq).toSeq, params.genIn))
      val unpackedOut = CustomFunctions.unpackOutputStream(params.genOut.getOrElse(params.genIn), params.lanes, out)
      //println(s"output = $unpackedOut")
      //println(s"window = ${params.window}")
      assert(unpackedOut.length == params.outputWindowSize)
      val expected_array = params.window.grouped(params.outputWindowSize).foldLeft(List.fill(params.outputWindowSize)(0.0)){case(x:Seq[Double], y:Seq[Double]) => x.zip(y).map{ case(a: Double, b: Double) => a+b}}
      //println(s"window sum = ${expected_array}")
      expected_array.zip(unpackedOut).zipWithIndex.foreach{ case ((expected, actual), j) => {
        // check if actual is within quantization of data = 2^-BP
        //println(s"Checking if $actual == $expected")
        if (actual > expected+pow(2,-16)*params.numTaps || actual < expected-pow(2,-16)*params.numTaps) {
          println(s"Error, mismatch on coefficient $j, expected $expected, got $actual")
          success = false
        }
      }}

      assert(success)
    }
  }

  coeffTest()


  //////////////////////////////////////////
  /////////// LEAKAGE TESTS ////////////////
  //////////////////////////////////////////

  // calculate the amount of leakage in neighboring bins and see if the PFB helps

  def toneTest(): Unit = {

    def getTone(numSamples: Int, f: Double): Seq[Double] = {
      (0 until numSamples).map(i => math.sin(2 * math.Pi * f * i))
    }

    def getEnergyAtBin(x_t: Seq[Double], bin: Int) : Double = {
      val fftSize = x_t.length
      val tform = fourierTr(DenseVector(x_t.toArray)).toArray.toSeq
      pow((tform(bin).abs / x_t.length), 2)
    }

    def simWindow[T <: Data : Ring](signal: Seq[Double], params: PFBParams[T]): Seq[Double] = {
      assert(signal.length == params.windowSize)
      val signalByWindow = signal.zip(params.window).map({case(x,y)=>x*y})
      (0 until params.outputWindowSize).map(i => {
        (0 until params.numTaps).foldLeft(0.0) { case(sum, j) => {
          sum + signalByWindow(i + j * params.outputWindowSize)
        }}
      })
    }

    def getSidelobeLevel(spectrum: Seq[Double]): Double = {
      val localMaxima = spectrum.toList.sliding(3).collect{ case a::b::c::Nil if a < b && b > c => b }.toList
      val m1 = localMaxima.reduceLeft(_ max _)
      val m2 = localMaxima.diff(List(m1)).reduceLeft(_ max _)
      10*log10(m1)-10*log10(m2)
    }

    it should s"reduce leakage" in {

      // setup params
      val params = PFBParams(
        genIn = FixedPoint(32.W, 28.BP),
        numTaps = 4,
        outputWindowSize = 256,
        windowFunc = blackmanHarris.apply,
        lanes = 16,
        convert = (x: Double) => FixedPoint.fromDouble(x, 32.W, 28.BP),
        outputPipelineDepth = 0,
        multiplyPipelineDepth = 0
      )

      // setup blind nodes 
      val inWidthBytes = (params.genIn.getWidth*params.lanes + 7)/ 8

      val numBins = 5
      val samples_per_bin = 10
      val windowSize = params.windowSize
      val fftSize    = params.outputWindowSize
      val testBin    =  fftSize / 6
      val delta_fs = (-numBins.toDouble to numBins.toDouble by (1.0 / samples_per_bin)).toSeq

      // no window 
      val rawResults = delta_fs map {delta_f => {
        getEnergyAtBin(getTone(fftSize, (testBin + delta_f) / fftSize), testBin)
      }}
      
      // floating-point simulation
      val simResults = delta_fs map {delta_f => {
        val tone = getTone(windowSize, (testBin + delta_f) / fftSize)
        val simPfb = simWindow(tone, params)
        //println(s"simPFB = $simPfb")
        getEnergyAtBin(simPfb, testBin)
      }}

      // chisel results
      val chiselResults = delta_fs map { delta_f => {
        val tone = getTone(params.lanes*params.processingDelay+params.outputWindowSize, (testBin + delta_f) / fftSize).grouped(params.lanes).toList
        //println(s"tone = $tone")
        //println(s"in = ${CustomFunctions.packInputStream(tone, params.genIn)}")
        val out = runTest(params, inWidthBytes, CustomFunctions.packInputStream(tone, params.genIn))
        val unpackedOut = CustomFunctions.unpackOutputStream(params.genOut.getOrElse(params.genIn), params.lanes, out)
        assert(unpackedOut.length == params.outputWindowSize)
        //println(s"chiselPFB = $unpackedOut")
        getEnergyAtBin(unpackedOut, testBin)
      }}

      println(s"delta  = $delta_fs")
      println(s"Raw    = $rawResults")
      println(s"Sim    = $simResults")
      println(s"Chisel = $chiselResults")

      // find leakage reduction
      val rawSL    = getSidelobeLevel(rawResults)
      val simSL    = getSidelobeLevel(simResults)
      val chiselSL = getSidelobeLevel(chiselResults)
      println(s"Original Sidelobe Level:  $rawSL dB")
      println(s"FP window Sidelobe Level: $simSL dB")
      println(s"Chisel Sidelobe Level:    $chiselSL dB")
      
    } 

  }

  toneTest()
  
  //////////////////////////////////////////
  /////////// OVERFLOW TESTS ///////////////
  //////////////////////////////////////////

  // check if the overflow bit correctly catches overflow

}



object CustomFunctions {
  import dsptools.{DspTester, DspException}

  def toBigIntUnsignedStevo(x: Double, totalWidth: Int, fractionalWidth: Int): BigInt = {
    val bi = FixedPoint.toBigInt(x, fractionalWidth)
    val neg = bi < 0
    val neededWidth = if (neg) bi.bitLength + 1 else bi.bitLength
    require(neededWidth <= totalWidth, "Double -> BigInt width larger than total width allocated!")
    if (neg) (((BigInt(1) << totalWidth) - 1) ^ -bi) + BigInt(1)
    else bi
  }


  // handle normal input types
  def packInputStream[T<:Data](in: Seq[Seq[Double]], gen: T): Seq[BigInt] = {
    gen match {
      case _: SInt | _: UInt =>
        in.map(x => x.reverse.foldLeft(BigInt(0)) { case (bi, dbl) =>
          val new_bi = BigInt(dbl.round.toInt)
          (bi << gen.getWidth) + new_bi
        })
      case f: FixedPoint =>
        f.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            in.map(x => x.reverse.foldLeft(BigInt(0)) { case (bi, dbl) =>
              val new_bi = toBigIntUnsignedStevo(dbl, f.getWidth, binaryPoint)
              (bi << gen.getWidth) + new_bi
            })
          case _ =>
            throw DspException(s"Error: packInput: Can't create FixedPoint from signal template $f")
        }
      case r: DspReal =>
        in.map(x => x.reverse.foldLeft(BigInt(0)) { case (bi, dbl) =>
          val new_bi = doubleToBigIntBits(dbl)
          (bi << gen.getWidth) + new_bi
        })
      case _ =>
        throw DspException(s"Error: packInput: Can't pack input type $gen yet...")
    }
  }

  //// handle complex input
  //def packInputStream[T<:Data](in: Seq[Seq[Complex]], gen: DspComplex[T]): Seq[BigInt] = {
  //  gen.underlyingType() match {
  //    case "SInt" =>
  //      in.map(x => x.reverse.foldLeft(BigInt(0)) { case (bi, cpx) =>
  //        val new_bi_real = BigInt(cpx.real.round.toInt)
  //        val new_bi_imag = BigInt(cpx.imag.round.toInt)
  //        (((bi << gen.real.getWidth) + new_bi_real) << gen.imag.getWidth) + new_bi_imag
  //      })
  //    case "fixed" =>
  //      gen.real.asInstanceOf[FixedPoint].binaryPoint match {
  //        case KnownBinaryPoint(binaryPoint) =>
  //          in.map(x => x.reverse.foldLeft(BigInt(0)) { case (bi, cpx) =>
  //            val new_bi_real = toBigIntUnsigned(cpx.real, gen.real.getWidth, binaryPoint)
  //            val new_bi_imag = toBigIntUnsigned(cpx.imag, gen.real.getWidth, binaryPoint)
  //            (((bi << gen.real.getWidth) + new_bi_real) << gen.imag.getWidth) + new_bi_imag
  //          })
  //        case _ =>
  //          throw DspException(s"Error: packInput: Can't create Complex[FixedPoint] from signal template ${gen.getClass.getName}")
  //      }
  //    case "real" =>
  //      in.map(x => x.reverse.foldLeft(BigInt(0)) { case (bi, cpx) =>
  //        val new_bi_real = doubleToBigIntBits(cpx.real)
  //        val new_bi_imag = doubleToBigIntBits(cpx.imag)
  //        (((bi << gen.real.getWidth) + new_bi_real) << gen.imag.getWidth) + new_bi_imag
  //      })
  //    case _ =>
  //      throw DspException(s"Error: packInput: DspComplex has unknown underlying type ${gen.getClass.getName}")
  //  }
  //}

   // unpack normal output data types
  def unpackOutputStream[T<:Data](gen: T, lanesOut: Int, streamOut: Seq[BigInt]): Seq[Double] = {
    gen match {
      case _:SInt | _:UInt =>
        streamOut.map(x => (0 until lanesOut).map{ idx => {
          ((x >> (gen.getWidth * idx)) % (BigInt(1) << gen.getWidth)).toDouble
        }}).flatten.toSeq
      case f: FixedPoint =>
        f.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            streamOut.map(x => (0 until lanesOut).map{ idx => {
              val y = (x >> (gen.getWidth * idx)) % (BigInt(1) << gen.getWidth)
              toDoubleFromUnsigned(y, gen.getWidth, binaryPoint)
            }}).flatten.toSeq
          case _ =>
            throw DspException(s"Error: packInput: Can't create FixedPoint from signal template $f")
        }
      case r: DspReal =>
        streamOut.map(x => (0 until lanesOut).map{ idx => {
          val y = (x >> (gen.getWidth * idx))
          bigIntBitsToDouble(y)
        }}).flatten.toSeq
      case _ =>
        throw DspException(s"Error: packInput: Can't unpack output type $gen yet...")
    }
  }

  //// unpack complex output data
  //def unpackOutputStream[T<:Data](gen: DspComplex[T], lanesOut: Int): Seq[Complex] = {
  //  gen.underlyingType() match {
  //    case "SInt" =>
  //      streamOut.map(x => (0 until lanesOut).map{ idx => {
  //        // TODO: doesn't work if width is > 32
  //        val imag = (x >> ((gen.real.getWidth + gen.imag.getWidth) * idx)) % pow(2, gen.imag.getWidth).toInt
  //        val real = (x >> ((gen.real.getWidth + gen.imag.getWidth) * idx + gen.imag.getWidth)) % pow(2, gen.real.getWidth).toInt
  //        Complex(real.toDouble, imag.toDouble)
  //      }}).flatten.toSeq
  //    case "fixed" =>
  //      gen.real.asInstanceOf[FixedPoint].binaryPoint match {
  //        case KnownBinaryPoint(binaryPoint) =>
  //          streamOut.map(x => (0 until lanesOut).map{ idx => {
  //            val imag = (x >> ((gen.real.getWidth + gen.imag.getWidth) * idx)) % pow(2, gen.imag.getWidth).toInt
  //            val real = (x >> ((gen.real.getWidth + gen.imag.getWidth) * idx + gen.imag.getWidth)) % pow(2, gen.real.getWidth).toInt
  //            Complex(toDoubleFromUnsigned(real, gen.real.getWidth, binaryPoint), toDoubleFromUnsigned(imag, gen.imag.getWidth, binaryPoint))
  //          }}).flatten.toSeq
  //        case _ =>
  //          throw DspException(s"Error: packInput: Can't create FixedPoint from signal template ${gen.getClass.getName}")
  //      }
  //    case "real" =>
  //      streamOut.map(x => (0 until lanesOut).map{ idx => {
  //        // [stevo]: comes out as (imag, real) because it's alphabetical
  //        val imag = (x >> ((gen.real.getWidth + gen.imag.getWidth) * idx))
  //        val real = (x >> ((gen.real.getWidth + gen.imag.getWidth) * idx + gen.imag.getWidth))
  //        Complex(bigIntBitsToDouble(real), bigIntBitsToDouble(imag))
  //      }}).flatten.toSeq
  //    case _ =>
  //      throw DspException(s"Error: packInput: DspComplex has unknown underlying type ${gen.getClass.getName}")
  //  }
  //}
}
