package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val text = string2Chars("dbfagccddacadfebbabffcbfbdgdddceefgdcgge")
    val tree = Fork(
                Fork(
                  Fork(
                    Leaf('e', 4), Leaf('a', 4), List('e', 'a'), 8
                  ),
                  Leaf('d', 9),
                  List('e', 'a', 'd'), 17),
                  Fork(
                    Fork(
                      Leaf('g', 5), Leaf('f', 6), List('g', 'f'), 11
                    ),
                    Fork(
                      Leaf('b', 6), Leaf('c', 6), List('b', 'c'), 12
                    ),
                    List('g', 'f', 'b', 'c'), 23
                  ),
                List('e', 'a', 'd', 'g', 'f', 'b', 'c'), 40)
    val bits = List(0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0)
    val table = List(('e', List(0, 0, 0)), ('a', List(0, 0, 1)), ('d', List(0, 1)), ('g', List(1, 0, 0)), ('f', List(1, 0, 1)), ('b', List(1, 1, 0)), ('c', List(1, 1, 1)))
	}

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("convert tree to table") {
    new TestTrees {
      assert(convert(tree) === table)
    }
  }

  test("encode text to bits") {
    new TestTrees {
      assert(encode(tree)(text) === bits)
    }
  }

  test("encode (quick) text to bits") {
    new TestTrees {
      assert(quickEncode(tree)(text) === bits)
    }
  }

  test("decode bits to text") {
    new TestTrees {
      assert(decode(tree, bits) === text)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val word = "abbaeea".toList
      assert(decode(tree, encode(tree)(word)) === word)
    }
  }

}
