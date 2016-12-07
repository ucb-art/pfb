// See LICENSE for license details.

package pfb

import cde._
import chisel3._
import dsptools._
import dsptools.numbers.{DspReal, Real}
import dsptools.numbers.implicits._
import _root_.junctions._
import uncore.tilelink._
import uncore.coherence._

class DspConfig extends Config(
  (pname, site, here) => pname match {
    case BuildDSP => q:Parameters =>
      implicit val p = q
      Module(new PFBBlock[DspReal])
    case PFBKey => PFBConfig()
    case NastiKey => NastiParameters(64, 32, 1)
    case PAddrBits => 32
    case CacheBlockOffsetBits => 6
    case AmoAluOperandBits => 64
    case TLId => "PFB"
    case TLKey("PFB") =>
        TileLinkParameters(
          coherencePolicy = new MICoherence(
            new NullRepresentation(1)),
          nManagers = 1,
          nCachingClients = 0,
          nCachelessClients = 1,
          maxClientXacts = 4,
          maxClientsPerPort = 1,
          maxManagerXacts = 1,
          dataBeats = 1,
          dataBits = 64)
    // case DspBlockKey => DspBlockParameters(-1, 1) //1024, 1024)
    case GenKey => new GenParameters {
      def getReal(): DspReal = DspReal(0.0).cloneType
      def genIn [T <: Data] = getReal().asInstanceOf[T]
      override def genOut[T <: Data] = getReal().asInstanceOf[T]
      val lanesIn = 8
      override val lanesOut = 8
    }
    case _ => throw new CDEMatchError
  })

case object PFBKey extends Field[PFBConfig]

trait HasPFBParameters[T <: Data] extends HasGenParameters[T, T] {
  implicit val p: Parameters
  val pfbConfig = p(PFBKey)
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
                      windowFunc: WindowConfig => Seq[Double] = sincHamming.apply,
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
