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
  def apply[T <: Data : Ring : ConvertableTo](
    id:String, pfbConfig: PFBConfig, gen: () => T): Config = new Config(
      (pname, site, here) => pname match {
        case PFBKey(id) => pfbConfig
        case IPXACTParameters(id) => {
          val parameterMap = Map[String, String]()
      
          // Conjure up some IPXACT synthsized parameters.
          val numTaps = pfbConfig.numTaps
          val gk = site(GenKey(site(DspBlockId)))
          val inputLanes = gk.lanesIn
          val outputLanes = gk.lanesOut
          val inputTotalBits = gk.genIn.getWidth * inputLanes
          val outputTotalBits = gk.genOut.getWidth * outputLanes
          parameterMap ++= List(("nTaps", numTaps.toString), ("InputLanes", inputLanes.toString),
            ("InputTotalBits", inputTotalBits.toString), ("OutputLanes", outputLanes.toString), ("OutputTotalBits", outputTotalBits.toString),
            ("OutputPartialBitReversed", "1"))
      
          // add fractional bits if it's fixed point
          gen () match {
            case fp: FixedPoint =>
              parameterMap ++= List(("InputFractionalBits", fp.binaryPoint.toString))
              parameterMap ++= List(("OutputFractionalBits", fp.binaryPoint.toString))
          }
          // Coefficients
          parameterMap ++= pfbConfig.window.zipWithIndex.map{case (coeff, index) => (s"FilterCoefficients$index", coeff.toString)}
          parameterMap ++= List(("FilterScale", "1"))
      
          // tech stuff, TODO
          parameterMap ++= List(("ClockRate", "100"), ("Technology", "TSMC16nm"))
      
          parameterMap
        }
      }) ++
    ConfigBuilder.dspBlockParams(id, pfbConfig.parallelism, gen)
  def standalone[T <: Data : Ring : ConvertableTo](id: String, pfbConfig: PFBConfig, gen: () => T): Config =
    apply(id, pfbConfig, gen) ++
    ConfigBuilder.buildDSP(id, {implicit p: Parameters => new LazyPFBBlock[T]})
}

class DefaultStandaloneRealPFBConfig extends Config(PFBConfigBuilder.standalone("pfb", PFBConfig(), () => DspReal()))

case class PFBKey(id: String) extends Field[PFBConfig]

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
  *                         Must be > 0 and a multiple of `parallelism`.
  * @param parallelism Number of parallel lanes in the FFT.
  * @param pipelineDepth Not currently used
  * @param useSinglePortMem Not currently used
  * @param symmetricCoeffs Not currently used
  * @param useDeltaCompression Not currently used
  */
case class PFBConfig(
                      val windowFunc: WindowConfig => Seq[Double] = sincHamming.apply,
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
  * Object used by [[PFB]] to generate window from [[PFBConfig]] parameters
  * @param numTaps
  * @param outputWindowSize
  */
case class WindowConfig(
                         numTaps: Int,
                         outputWindowSize: Int
)
