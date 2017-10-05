package pfb

import breeze.math.Complex
import chisel3._
import chisel3.iotesters.PeekPokeTester
import dspblocks.{CSRField, PeekPokePackers}
import dsptools.numbers._
import dsptools.numbers.implicits._
import freechips.rocketchip.amba.axi4stream.AXI4StreamBundlePayload
import amba.axi4._
import freechips.rocketchip.tilelink._

import scala.collection._
import scala.util.Random
import scala.language.implicitConversions

case class PeekPoke[T, V](
                      peek: T => V,
                      poke: V => T
                      )

class PFBTester[T <: Data : Ring, V](c: PFBBlindModule[T]) extends PeekPokeTester(c) 
  with AXI4MasterModel[PFBBlindModule[T]] {

  val memAXI = c.io.mem(0)

  val csrs = c.outer.pfb.csrs.module.addrmap
  axiReset()
}

class PFBDataTester[T <: Data : Ring](c: PFBBlindModule[T], in: Seq[BigInt], 
  out: mutable.ArrayBuffer[BigInt]) extends PFBTester(c) {

  val io_in  = c.io.in(0)
  val io_out = c.io.out(0)

  poke(io_in.valid, 1)
  poke(io_in.bits.last, 0)
  poke(io_out.ready, 1)

  in.foreach { x =>
    poke(io_in.valid, 1)
    // TODO tlast, etc.
    poke(io_in.bits.data, x)
    if (peek(io_out.valid) != BigInt(0)) {
      out.append(peek(io_out.bits.data))
    }
    step(1)
  }
}
