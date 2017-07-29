package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] =
    for {
      i <- arbitrary[A]
      h <- frequency((3, genHeap), (1, const(empty)))
    } yield insert(i, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min of 2 heaps melded to new heap and deleted is equal to old heap") = forAll { (h1: H, h2: H) =>
    def min(h1: H, h2: H) = {
      val (a1, a2) = (findMin(h1), findMin(h2))
      if(a1 < a2) (h2, a1) else (h1, a2)
    }
    val (h, a) = min(h1, h2)
    val ha = insert(a, empty)

    deleteMin(meld(h, ha)) == h && deleteMin(meld(ha, h)) == h
  }

  property("delete has same effect on composition of 2 heaps") = forAll { (h1: H, h2: H) =>
    def elements(h: H): List[A] = {
      if (isEmpty(h)) List.empty
      else findMin(h) :: elements(deleteMin(h))
    }
    val (xs1, xs2) = (elements(h1), elements(h2))
    val xs = xs1 ::: xs2
    val h = meld(h1, h2)

    elements(h) == xs.sorted
  }
}
