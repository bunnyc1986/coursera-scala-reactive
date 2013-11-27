package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "or 4")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "or 4")
  }

  test("demux example") {
    val in, c1, c2, out1, out2, out3, out4 = new Wire
    val c = List[Wire](c1, c2)
    val out = List[Wire](out1, out2, out3, out4)
    demux(in, c, out)
    in.setSignal(false)
    c1.setSignal(false)
    c2.setSignal(false)
    run

    assert(out1.getSignal === false, "demux 1 out1")
    assert(out2.getSignal === false, "demux 1 out2")
    assert(out3.getSignal === false, "demux 1 out3")
    assert(out4.getSignal === false, "demux 1 out4")

    in.setSignal(true)
    run

    assert(out1.getSignal === false, "demux 2 out1")
    assert(out2.getSignal === false, "demux 2 out2")
    assert(out3.getSignal === false, "demux 2 out3")
    assert(out4.getSignal === true, "demux 2 out4")

    c1.setSignal(true)
    run

    assert(out1.getSignal === false, "demux 3 out1")
    assert(out2.getSignal === true, "demux 3 out2")
    assert(out3.getSignal === false, "demux 3 out3")
    assert(out4.getSignal === false, "demux 3 out4")

    c1.setSignal(false)
    c2.setSignal(true)
    run

    assert(out1.getSignal === false, "demux 4 out1")
    assert(out2.getSignal === false, "demux 4 out2")
    assert(out3.getSignal === true, "demux 4 out3")
    assert(out4.getSignal === false, "demux 4 out4")

    c1.setSignal(true)
    run

    assert(out1.getSignal === true, "demux 5 out1")
    assert(out2.getSignal === false, "demux 5 out2")
    assert(out3.getSignal === false, "demux 5 out3")
    assert(out4.getSignal === false, "demux 5 out4")

    c2.setSignal(true)
    run

    assert(out1.getSignal === true, "demux 6 out1")
    assert(out2.getSignal === false, "demux 6 out2")
    assert(out3.getSignal === false, "demux 6 out3")
    assert(out4.getSignal === false, "demux 6 out4")
  }

}
