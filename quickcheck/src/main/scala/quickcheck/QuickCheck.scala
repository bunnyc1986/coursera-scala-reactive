package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val r = findMin(h)
    r == { if (a < b) a else b }
  }

  property("asc") = forAll { (s: List[Int]) =>
    val expected = s.sortBy(e => e)
    val h = insertAll(s, empty)
    inSeqOf(h, expected)
  }

  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    isEmpty(h1)
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val meldHeap = meld(h1, h2)
    val expected = (toList(h1) ++ toList(h2)).sortBy(e => e)
    inSeqOf(meldHeap, expected)
  }

  def toList(h: H) = {
    def _toList(h: H, acc: List[Int]): List[Int] = {
      if (isEmpty(h)) acc
      else _toList(deleteMin(h), findMin(h) :: acc)
    }
    _toList(h, List()).reverse
  }

  def inSeqOf(h: H, l: List[Int]): Boolean = {
    if (isEmpty(h) && l.isEmpty) true
    else if ((isEmpty(h) || l.isEmpty)) false
    else if (findMin(h) == l.head) inSeqOf(deleteMin(h), l.tail)
    else false
  }

  def insertAll(s: List[Int], h: H): H = {
    if (s.isEmpty) h
    else insert(s.head, insertAll(s.tail, h))
  }

}
