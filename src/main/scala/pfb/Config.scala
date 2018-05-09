// See LICENSE for license details.

package pfb

import cde._
import chisel3._
import chisel3.experimental._
import craft._
import dsptools._
import dsptools.numbers.{Field=>_,_}
import dsptools.numbers.implicits._
import dspblocks._
import dspjunctions._
import dspblocks._
import _root_.junctions._
import uncore.tilelink._
import uncore.coherence._
import scala.collection.mutable.Map


object PFBConfigBuilder {
  def apply[T <: Data : Ring, V](
    id: String, pfbConfig: PFBConfig, genIn: () => T, convert: Double => T, genOut: Option[() => T] = None, genTap: Option[T] = None): Config = new Config(
      (pname, site, here) => pname match {
        case PFBConvert(_id) if _id == id => convert
        case PFBKey(_id)     if _id == id => pfbConfig.copy(genTap = genTap)
        case IPXactParameters(_id) if _id == id => {
          val parameterMap = Map[String, String]()

          // Conjure up some IPXACT synthsized parameters.
          val numTaps = pfbConfig.numTaps
          val outputWindowSize = pfbConfig.outputWindowSize
          val gk = site(GenKey(id))
          val inputLanes = gk.lanesIn
          val outputLanes = gk.lanesOut
          parameterMap ++= List(
            ("nBands", outputWindowSize.toString),
            ("nTotalTaps", (outputWindowSize*numTaps).toString),
            ("InputLanes", inputLanes.toString)
          )

          // add fractional bits if it's fixed point
          genIn() match {
            case fp: FixedPoint =>
              val fractionalBits = fp.binaryPoint
              parameterMap ++= List(
                ("InputFractionalBits", fractionalBits.get.toString),
                ("InputTotalBits", fp.getWidth.toString)
              )
            case c: DspComplex[_] =>
              parameterMap ++= List(
                ("InputTotalBits", c.real.getWidth.toString) // assume real and imag have equal total widths
              )
              c.underlyingType() match {
                case "fixed" =>
                  val fractionalBits = c.real.asInstanceOf[FixedPoint].binaryPoint
                  parameterMap ++= List(
                    ("InputFractionalBits", fractionalBits.get.toString)
                  )
                case _ => 
              }
            case d: DspReal =>
              parameterMap ++= List(
                ("InputTotalBits", d.getWidth.toString)
              )
            case s: SInt => 
              parameterMap ++= List(
                ("InputTotalBits", s.getWidth.toString)
              )
            case _ =>
              throw new DspException("Unknown input type for PFB")
          }

          // add fractional bits if it's fixed point
          genOut.getOrElse(genIn)() match {
            case fp: FixedPoint =>
              val fractionalBits = fp.binaryPoint
              parameterMap ++= List(
                ("OutputFractionalBits", fractionalBits.get.toString),
                ("OutputTotalBits", fp.getWidth.toString)
              )
            case c: DspComplex[_] =>
              parameterMap ++= List(
                ("OutputTotalBits", c.real.getWidth.toString) // assume real and imag have equal total widths
              )
              c.underlyingType() match {
                case "fixed" =>
                  val fractionalBits = c.real.asInstanceOf[FixedPoint].binaryPoint
                  parameterMap ++= List(
                    ("OutputFractionalBits", fractionalBits.get.toString)
                  )
                case _ => 
              }
            case d: DspReal =>
              parameterMap ++= List(
                ("OutputTotalBits", d.getWidth.toString)
              )
            case s: SInt => 
              parameterMap ++= List(
                ("OutputTotalBits", s.getWidth.toString)
              )
            case _ =>
              throw new DspException("Unknown output type for PFB")
          }
          // add fractional bits if it's fixed point
          genTap.getOrElse(genIn()) match {
            case fp: FixedPoint =>
              val fractionalBits = fp.binaryPoint
              parameterMap ++= List(
                ("FilterCoefficientFractionalBits", fractionalBits.get.toString),
                ("FilterCoefficientTotalBits", fp.getWidth.toString)
              )
            case c: DspComplex[_] =>
              parameterMap ++= List(
                ("FilterCoefficientTotalBits", c.real.getWidth.toString) // assume real and imag have equal total widths
              )
              c.underlyingType() match {
                case "fixed" =>
                  val fractionalBits = c.real.asInstanceOf[FixedPoint].binaryPoint
                  parameterMap ++= List(
                    ("FilterCoefficientFractionalBits", fractionalBits.get.toString)
                  )
                case _ => 
              }
            case d: DspReal =>
              parameterMap ++= List(
                ("FilterCoefficientTotalBits", d.getWidth.toString)
              )
            case s: SInt => 
              parameterMap ++= List(
                ("FilterCoefficientTotalBits", s.getWidth.toString)
              )
            case _ =>
              throw new DspException("Unknown coefficient type for PFB")
          }

          // Coefficients
          parameterMap ++= pfbConfig.window.zipWithIndex.map{case (coeff, index) => (s"FilterCoefficients_$index", coeff.toString)}
          parameterMap ++= List(("ProcessingDelay", pfbConfig.processingDelay.toString))

          // tech stuff, TODO
          parameterMap ++= List(("ClockRate", "100"), ("Technology", "TSMC16nm"))

          parameterMap
        }
        case _ => throw new CDEMatchError
      }) ++
  ConfigBuilder.genParams(id, pfbConfig.lanes, genIn, genOutFunc = genOut)
  def standalone[T <: Data : Ring](id: String, pfbConfig: PFBConfig, genIn: () => T, convert: Double => T, genOut: Option[() => T] = None, genTap: Option[T] = None): Config =
    apply(id, pfbConfig, genIn, convert, genOut, genTap) ++
    ConfigBuilder.buildDSP(id, {implicit p: Parameters => new PFBBlock[T]})
}

// default floating point and fixed point configurations
class DefaultStandaloneRealPFBConfig extends Config(PFBConfigBuilder.standalone("pfb", PFBConfig(), () => DspReal(), d => DspReal(d)))
class DefaultStandaloneFixedPointPFBConfig extends Config(PFBConfigBuilder.standalone("pfb", PFBConfig(), () => FixedPoint(32.W, 16.BP), d => FixedPoint.fromDouble(d, 32.W, 16.BP)))

// provides a sample custom configuration
class CustomStandalonePFBConfig extends Config(PFBConfigBuilder.standalone(
  "pfb",
  PFBConfig(
    // must be numTaps * outputWindowSize in length
    windowFunc= userCoeff.apply,
    numTaps=12,
    outputWindowSize=128,
    lanes=4,
    processingDelay = 192,
    outputPipelineDepth = 1,
    multiplyPipelineDepth = 1
    ),
  genIn = () => DspComplex(FixedPoint(11.W, 10.BP), FixedPoint(11.W, 10.BP)),
  genOut = Some(() => DspComplex(FixedPoint(12.W, 17.BP), FixedPoint(12.W, 17.BP))),
  genTap = Some(DspComplex(FixedPoint(12.W, 17.BP), FixedPoint(12.W, 17.BP))),
  convert = d => DspComplex(FixedPoint.fromDouble(d, 12.W, 17.BP), FixedPoint.fromDouble(d, 12.W, 17.BP))
))

case class PFBConvert(id: String) extends Field[Double => Data]
case class PFBKey(id: String) extends Field[PFBConfig]

// by default, the taps have no data type, which defaults to the input data type
trait HasPFBParameters[T <: Data] extends HasGenParameters[T, T] {
  implicit val p: Parameters
  val pfbConfig = p(PFBKey(p(DspBlockId)))
  def genTap: Option[T] = pfbConfig.genTap.asInstanceOf[Option[T]]
  def convert(x: Double): T = p(PFBConvert(p(DspBlockId)))(x).asInstanceOf[T]
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
                      processingDelay: Int = 10,
                      outputPipelineDepth: Int = 1,
                      multiplyPipelineDepth: Int = 1,
                      quadrature: Boolean = true,
                    // the below are currently ignored
                      useSinglePortMem: Boolean = false,
                      symmetricCoeffs: Boolean  = false,
                      useDeltaCompression: Boolean = false,
                      genTap: Option[Data] = None
                    ) {

  def scaleWindow(window: Seq[Double], genTap: Data): Seq[Double] = {

    var min = 0.0
    var max = 0.0
    var res = 0.0

    genTap match {
      case fp: FixedPoint =>
        val fractionalBits = fp.binaryPoint.get
        val totalBits = fp.getWidth
        res = fractionalBits
        max = math.pow(2, totalBits-1)-1
        min = -max-res
      case _ =>
        throw new DspException("Unknown coefficient type for PFB")
    }

    val window_min = window.reduceLeft(math.min)
    val window_max = window.reduceLeft(math.max)
    return window.map { x => math.round((x/window_max * max)) * math.pow(2, -res) }

  }

  val window = scaleWindow(windowFunc( WindowConfig(numTaps, outputWindowSize) ), genTap.get)
  val windowSize = window.length

  val lanes_new = if (quadrature) lanes/2 else lanes

  // various checks for validity
  require(numTaps > 0, "Must have more than zero taps")
  require(outputWindowSize > 0, "Output window must have size > 0")
  require(outputWindowSize % lanes_new == 0, "Number of parallel inputs must divide the output window size")
  require(windowSize > 0, "PFB window must have > 0 elements")
  require(windowSize == numTaps * outputWindowSize, "windowFunc must return a Seq() of the right size")
  require(processingDelay >= 0, "Must have positive processing delay")
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
