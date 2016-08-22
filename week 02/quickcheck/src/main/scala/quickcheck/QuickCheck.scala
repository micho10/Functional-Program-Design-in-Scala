package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") =
    forAll { h: H =>
      val m = if (isEmpty(h)) 0 else findMin(h)
      findMin(insert(m, h)) == m
    }

  property("the minimum of a heap with 1 element is said element") =
    forAll { a: Int =>
      findMin(insert(a, empty)) == a
    }

  property("insert any 2 elements into an empty heap. Finding the minimum shall return the smallest of the 2 elements") =
    forAll { (a: Int, b: Int) =>
      val h = insert(b, insert(a, empty))
      findMin(h) == min(a, b)
    }

  property("insert an element into an empty heap and delete one element from it should produce an empty heap") =
    forAll { a: Int =>
      isEmpty(deleteMin(insert(a, empty)))
    }

  property("continually finding and deleting the minimum should produce a sorted sequence") =
    forAll {
      h: H =>
        def isSorted(h: H): Boolean =
          if (isEmpty(h)) true
          else {
            val minimum = findMin(h)
            val h2 = deleteMin(h)
            isEmpty(h2) || (minimum <= findMin(h2) && isSorted(h2))
          }
        isSorted(h)
    }

  property("the minimum of the meld of 2 heaps should be the minimum of both heaps") =
    forAll {
      (h1: H, h2: H) =>
        findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
    }

  property("2 heaps should be equal if continually removing minimum elements results in equal heaps") =
    forAll {
      (h1: H, h2: H) =>
        def heapEqual(h1: H, h2: H): Boolean =
          if (isEmpty(h1) && isEmpty(h2)) true
          else {
            val minimum1 = findMin(h1)
            val minimum2 = findMin(h2)
            (minimum1 == minimum2) && heapEqual(deleteMin(h1), deleteMin(h2))
          }
        heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))

    }

}