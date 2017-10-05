package pfb

import chisel3._
import chisel3.core.FixedPoint
import chisel3.internal.firrtl.Width
import chisel3.iotesters._
import chisel3.util._
import dspblocks._
import dsptools.numbers._
import dsptools.numbers.implicits._
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


import breeze.math.Complex
import chisel3.internal.firrtl.KnownBinaryPoint
import dsptools.DspTesterUtilities._
import dsptools.{DspTester, DspException}
import scala.math.{abs, pow}




class PFBSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.root((new BaseCoreplexConfig).toInstance)

  def runTest[T <: Data : Ring](params: PFBParams[T], 
                                blindNodes: DspBlock.AXI4BlindNodes,
                                in: Seq[BigInt]): Seq[BigInt] = {
    val out = new mutable.ArrayBuffer[BigInt]()

    iotesters.Driver.execute(
      Array(/*"-tiv",*/ "-tbn", "firrtl", "-twffn", "out.vcd", "-fiwv"),
      () => LazyModule(new PFBBlind(params, blindNodes)).module
    ) { c =>
      new PFBDataTester(c, in, out)
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
        address = AddressSet(0x0, 0xffffffffL),
        beatBytes = 8,
        numTaps = numTaps,
        outputWindowSize = outputWindowSize,
        windowFunc = winFunc,
        lanes = lanes,
        convert = convert 
      )

      // setup blind nodes 
      val inWidthBytes = (params.genIn.getWidth*params.lanes + 7)/ 8
      val outWidthBytes = params.genOut.map(x => (x.getWidth*params.lanes + 7)/8).getOrElse(inWidthBytes)
      println(s"In bytes = $inWidthBytes and out bytes = $outWidthBytes")
      val blindNodes = DspBlockBlindNodes(
        streamIn  = () => AXI4StreamBlindInputNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("pfb", n = inWidthBytes))))),
        streamOut = () => AXI4StreamBlindOutputNode(Seq(AXI4StreamSlavePortParameters())),
        mem       = () => AXI4BlindInputNode(Seq(AXI4MasterPortParameters(Seq(AXI4MasterParameters("pfb")))))
      )

      // setup fake test data and run test
      val values = CustomFunctions.packInputStream(Seq.fill(10)(Seq.fill(params.lanes)(0.0)), params.genIn)
      val out = runTest(params, blindNodes, values)
    }
  }

  paramTest(genIn = UInt(32.W), convert = (x: Double) => x.toInt.U)
  paramTest(genIn = SInt(32.W), convert = (x: Double) => x.toInt.S)
  // doesn't work yet
  //paramTest(genIn = DspComplex(SInt(32.W), SInt(32.W)))
  paramTest(genIn = FixedPoint(32.W, 16.BP), convert = (x: Double) => FixedPoint.fromDouble(x, 32.W, 16.BP))
  // downsize coefficients to fit in 16 bits (32 BP)
  paramTest(genIn = FixedPoint(16.W, 32.BP), convert = (x: Double) => FixedPoint.fromDouble(x/math.pow(2, 32), 16.W, 32.BP))
  paramTest(winFunc = sincHamming.apply, winFuncName = "Sinc + Hamming")
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

    it should s"return the data with all coefficients as 1" in {

      // setup params
      val params = PFBParams(
        genIn = UInt(32.W),
        address = AddressSet(0x0, 0xffffffffL),
        beatBytes = 8,
        numTaps = 4,
        outputWindowSize = 256,
        windowFunc = onesWindow.apply,
        lanes = 16,
        convert = (x: Double) => x.toInt.U
      )

      // setup blind nodes 
      val inWidthBytes = (params.genIn.getWidth*params.lanes + 7)/ 8
      val outWidthBytes = params.genOut.map(x => (x.getWidth*params.lanes + 7)/8).getOrElse(inWidthBytes)
      println(s"In bytes = $inWidthBytes and out bytes = $outWidthBytes")
      val blindNodes = DspBlockBlindNodes(
        streamIn  = () => AXI4StreamBlindInputNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("pfb", n = inWidthBytes))))),
        streamOut = () => AXI4StreamBlindOutputNode(Seq(AXI4StreamSlavePortParameters())),
        mem       = () => AXI4BlindInputNode(Seq(AXI4MasterPortParameters(Seq(AXI4MasterParameters("pfb")))))
      )

      // setup test data
      val values = Seq.fill(10)(Seq.fill(params.lanes)(1.0))
      println(s"input = $values")
      val out = runTest(params, blindNodes, CustomFunctions.packInputStream(values, params.genIn))
      val unpackedOut = CustomFunctions.unpackOutputStream(params.genOut.getOrElse(params.genIn), params.lanes, out)
      println(s"output = $unpackedOut")
    }
  }

  coeffTest()


  //////////////////////////////////////////
  /////// SINGLE TONE TESTS ////////////////
  //////////////////////////////////////////

  // calculate the amount of leakage in neighboring bins and see if the PFB helps

  def toneTest[T <: Data : Ring](params: PFBParams[T], freq: Int, amp: Int): Unit = {

    it should s"reduce leakage" in {
      // setup blind nodes 
      val inWidthBytes = (params.genIn.getWidth*params.lanes + 7)/ 8
      val outWidthBytes = params.genOut.map(x => (x.getWidth*params.lanes + 7)/8).getOrElse(inWidthBytes)
      println(s"In bytes = $inWidthBytes and out bytes = $outWidthBytes")
      val blindNodes = DspBlockBlindNodes(
        streamIn  = () => AXI4StreamBlindInputNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("pfb", n = inWidthBytes))))),
        streamOut = () => AXI4StreamBlindOutputNode(Seq(AXI4StreamSlavePortParameters())),
        mem       = () => AXI4BlindInputNode(Seq(AXI4MasterPortParameters(Seq(AXI4MasterParameters("pfb")))))
      )
    } 

  }
  
  //////////////////////////////////////////
  /////////// OVERFLOW TESTS ///////////////
  //////////////////////////////////////////

  // check if the overflow bit correctly catches overflow

}



object CustomFunctions {
  import dsptools.{DspTester, DspException}
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
              val new_bi = toBigIntUnsigned(dbl, f.getWidth, binaryPoint)
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
              // TODO: doesn't work if width is >= 32
              val y = (x >> (gen.getWidth * idx)) % pow(2, gen.getWidth).toInt
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
