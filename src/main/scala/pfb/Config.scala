// See LICENSE for license details.

package pfb

import cde._
import chisel3._
import chisel3.experimental._
import craft._
import dsptools._
import dsptools.numbers.{Field=>_,_}
import dsptools.numbers.implicits.{ConvertableToDspComplex=>_,_}
import dspblocks._
import dspjunctions._
import dspblocks._
import _root_.junctions._
import uncore.tilelink._
import uncore.coherence._
import scala.collection.mutable.Map

object ConvertableToDspComplex extends
    dsptools.numbers.ConvertableToDspComplex[FixedPoint] {
  override def fromType[B](n: B)(implicit c: ConvertableFrom[B]): DspComplex[FixedPoint] = {
    fromDouble(c.toDouble(n))
  }
  override def fromDouble(n: Double): DspComplex[FixedPoint] = {
    DspComplex.wire(FixedPoint.fromDouble(n, 32.W, 16.BP), FixedPoint.fromDouble(0.0, 1.W, 0.BP))
  }
}

object myimplicit {
 implicit val convertableTo = ConvertableToDspComplex
}
import myimplicit._

object PFBConfigBuilder {
  def apply[T <: Data : Ring : ConvertableTo](
    id: String, pfbConfig: PFBConfig, genIn: () => T, genOut: Option[() => T] = None): Config = new Config(
      (pname, site, here) => pname match {
        case PFBKey(_id) if _id == id => pfbConfig
        case IPXactParameters(_id) if _id == id => {
          val parameterMap = Map[String, String]()
      
          // Conjure up some IPXACT synthsized parameters.
          val numTaps = pfbConfig.numTaps
          val gk = site(GenKey(id))
          val inputLanes = gk.lanesIn
          val outputLanes = gk.lanesOut
          val inputTotalBits = gk.genIn.getWidth * inputLanes
          val outputTotalBits = gk.genOut.getWidth * outputLanes
          parameterMap ++= List(
            ("nTaps", numTaps.toString), 
            ("InputLanes", inputLanes.toString),
            ("InputTotalBits", inputTotalBits.toString), 
            ("OutputLanes", outputLanes.toString), 
            ("OutputTotalBits", outputTotalBits.toString),
            ("OutputPartialBitReversed", "1")
          )
      
          // add fractional bits if it's fixed point
          genIn() match {
            case fp: FixedPoint =>
              val fractionalBits = fp.binaryPoint
              parameterMap ++= List(
                ("InputFractionalBits", fractionalBits.get.toString)
              )
            case c: DspComplex[T] =>
              c.underlyingType() match {
                case "fixed" =>
                  val fractionalBits = c.real.asInstanceOf[FixedPoint].binaryPoint
                  parameterMap ++= List(
                    ("InputFractionalBits", fractionalBits.get.toString)
                  )
                case _ => 
              }
            case _ =>
          }
          genOut.getOrElse(genIn)() match {
            case fp: FixedPoint =>
              val fractionalBits = fp.binaryPoint
              parameterMap ++= List(
                ("OutputFractionalBits", fractionalBits.get.toString)
              )
            case c: DspComplex[T] =>
              c.underlyingType() match {
                case "fixed" =>
                  val fractionalBits = c.real.asInstanceOf[FixedPoint].binaryPoint
                  parameterMap ++= List(
                    ("OutputFractionalBits", fractionalBits.get.toString)
                  )
                case _ => 
              }
            case _ =>
          }

          // Coefficients
          parameterMap ++= pfbConfig.window.zipWithIndex.map{case (coeff, index) => (s"FilterCoefficients$index", coeff.toString)}
          parameterMap ++= List(("FilterScale", "1"))
      
          // tech stuff, TODO
          parameterMap ++= List(("ClockRate", "100"), ("Technology", "TSMC16nm"))
      
          parameterMap
        }
        case _ => throw new CDEMatchError
      }) ++
  ConfigBuilder.genParams(id, pfbConfig.lanes, genIn, genOutFunc = genOut)
  def standalone[T <: Data : Ring : ConvertableTo](id: String, pfbConfig: PFBConfig, genIn: () => T, genOut: Option[() => T] = None): Config =
    apply(id, pfbConfig, genIn, genOut) ++
    ConfigBuilder.buildDSP(id, {implicit p: Parameters => new PFBBlock[T]})
}

// default floating point and fixed point configurations
class DefaultStandaloneRealPFBConfig extends Config(PFBConfigBuilder.standalone("pfb", PFBConfig(), () => DspReal()))
class DefaultStandaloneFixedPointPFBConfig extends Config(PFBConfigBuilder.standalone("pfb", PFBConfig(), () => FixedPoint(32.W, 16.BP)))
class DefaultStandaloneComplexPFBConfig extends Config(PFBConfigBuilder.standalone("pfb", PFBConfig(), () => DspComplex(FixedPoint(32.W, 16.BP), FixedPoint(32.W, 16.BP))))

// provides a sample custom configuration
class CustomStandalonePFBConfig extends Config(PFBConfigBuilder.standalone(
  "pfb", 
  PFBConfig(
    // must be numTaps * outputWindowSize in length
    windowFunc= (w: WindowConfig) => Seq.tabulate(w.numTaps*w.outputWindowSize)(i => scala.math.sin(i)),
    numTaps=23, 
    outputWindowSize=32, 
    lanes=16), 
  genIn = () => DspComplex(FixedPoint(18.W, 16.BP), FixedPoint(18.W, 16.BP)),
  genOut = Some(() => DspComplex(FixedPoint(20.W, 16.BP), FixedPoint(20.W, 16.BP)))
))

case class PFBKey(id: String) extends Field[PFBConfig]

// by default, the taps have no data type, which defaults to the input data type
trait HasPFBParameters[T <: Data] extends HasGenParameters[T, T] {
  implicit val p: Parameters
  val pfbConfig = p(PFBKey(p(DspBlockId)))
  def genTap: Option[T] = None
}

/**
  * Case class for holding PFB configuration information
  * @param windowFunc A function that generates a window given window a [[WindowConfig]],
  *                   which includes output window size and and number of taps.
  *                   Must give a window of size `numTaps * outputWindowSize`.
  * @param numTaps Number of taps, used when calling windowFunc. Must be > 0.
  * @param outputWindowSize Size of the output window, often the same as the size of an FFT following the PFB.
  *                         Must be > 0 and a multiple of `lanes`.
  * @param lanes Number of parallel lanes in the FFT.
  * @param pipelineDepth Not currently used
  * @param useSinglePortMem Not currently used
  * @param symmetricCoeffs Not currently used
  * @param useDeltaCompression Not currently used
  */
case class PFBConfig(
                      val windowFunc: WindowConfig => Seq[Double] = sincHamming.apply,
                      numTaps: Int = 4,
                      outputWindowSize: Int = 16,
                      lanes: Int = 8,
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
