package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: toList(deleteMin(h))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("Find Min") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == math.min(a, b)
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("Delete Min from a Heap with only one element must be an empty heap") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("Heap must return always the min value calling deleteMin") = forAll { (h1: H) =>
    val xs = toList(h1)
    xs == xs.sorted
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("Min of two heaps must be the min of the two heaps melded") = forAll { (h1: H, h2: H) =>
    val minH1 = findMin(h1)
    val minH2 = findMin(h2)
    findMin(meld(h1, h2)) == math.min(minH1, minH2)
  }

  property("Delete Min / Find Min") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(deleteMin(h)) == math.max(a, b)
  }

  property("Meld must be associative") = forAll { (h1: H, h2: H, h3: H) =>
    val meld1 = meld(meld(h1, h2), h3)
    val meld2 = meld(h1, meld(h2, h3))

    toList(meld1) == toList(meld2)
  }

  property("Insert min after delete it must be the heap min") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, deleteMin(h))) == m
  }

  property("Delete Min / Find Min") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(deleteMin(h)) == math.max(a, b)
  }

  property("Meld two empty Heaps must be an empty heap") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    val emptyH1 = deleteMin(h1)
    val emptyH2 = deleteMin(h2)
    isEmpty(meld(emptyH1, emptyH2))
  }

  property("Delete two elements from a meld Heap with two elements must be an empty heap") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    val h3 = meld(h1, h2)
    isEmpty(deleteMin(deleteMin(h3)))
  }

  property("Two melded heaps must return always the min value calling deleteMin") = forAll { (h1: H, h2: H) =>
    val minH1 = findMin(h1)
    val meldOrg = meld(h1, h2)
    val meldDest = meld(deleteMin(h1), insert(minH1, h2))

    toList(meldOrg) == toList(meldDest)
  }

}
