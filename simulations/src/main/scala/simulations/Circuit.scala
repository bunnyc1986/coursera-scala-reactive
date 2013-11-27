package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () =>
        afterDelay(0) {
          println(
            "  " + currentTime + ": " + name + " -> " + wire.getSignal)
        }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val w1, w2, w3 = new Wire
    inverter(a1, w1)
    inverter(a2, w2)
    andGate(w1, w2, w3)
    inverter(w3, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case Nil => andGate(in, in, out(0))
      case x :: xs => {
        // Refer to the diagram
        val inL, inR, notX = new Wire
        andGate(in, x, inL)
        inverter(x, notX); andGate(in, notX, inR)

        val n = out.length / 2
        demux(inL, xs, out take n)
        demux(inR, xs, out drop n)
      }
    }
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGate2Example {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def demuxExample {
    val in, c1, c2, c3, out1, out2, out3 = new Wire
    val c = List[Wire](c1, c2, c3)
    val out = List[Wire](out1, out2, out3)
    demux(in, c, out)
    probe("in", in)
    probe("c1", c1)
    probe("c2", c2)
    probe("c3", c3)
    probe("out1", out1)
    probe("out2", out2)
    probe("out3", out3)
    in.setSignal(false)
    c1.setSignal(false)
    c2.setSignal(false)
    c3.setSignal(false)
    run

    in.setSignal(true)
    run

    in.setSignal(true)
    c1.setSignal(true)
    run

    c1.setSignal(false)
    c2.setSignal(true)
    run

    c2.setSignal(false)
    c3.setSignal(true)
    run

    c3.setSignal(false)
    run
  }
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample

  Circuit.orGateExample

  Circuit.orGate2Example

  Circuit.demuxExample
}
