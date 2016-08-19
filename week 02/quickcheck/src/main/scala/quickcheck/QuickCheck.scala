package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minimum of 1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minimum of 2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val e1 = findMin(h)
    val e2 = findMin(deleteMin(h))
    e1 <= e2
  }

  property("delete last element in the heap should produce an empty heap") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

  property("continually finding and deleting the minimum produces a sorted sequence") = ???

  property("the minimum of the meld of 2 heaps should be the minimum of 1 of them") = ???

}
