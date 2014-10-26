package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("test times") {
    val chars = List('a', 'b', 'c', 'd', 'd', 'e', 'd')
    assert(times(chars) == List(('a', 1), ('b', 1), ('c', 1), ('d', 3), ('e', 1)))
  }

  def m1(p1:Int, p2:Int)(p3:Int) = {
    println("p1=%s, p2=%s, p3=%s", p1, p2, p3)
  }

  test("test collections") {
    val chars = List('a', 'b', 'c', 'd', 'd', 'e', 'd')
    println(pack(chars))
    println(times(chars))
    println(chars.groupBy(c => c).mapValues(l => l.length).toList)

    val m = m1(1,2)_
    m(3)

    println(makeOrderedLeafList(times(chars)))
    println(combine(makeOrderedLeafList(times(chars))))

    println("====")
    val k = List(55,66,77)
    k match {
      case s::Nil => println(s)
      case s::st=> println(" s=" + s + ", st="+st)
    }

    println(k splitAt 2)

    val d = List('a', 'b')
    println('d' :: d)
    println(d ::: List('d'))
  }

  test("test french decode") {
    println(decode(frenchCode, secret))
  }

  test("test encode decode for french code") {
    assert(encode(frenchCode)(decode(frenchCode, secret)) === secret)
  }

  test("test quick encode for french code") {
    println("Encode:\t" + encode(frenchCode)(decode(frenchCode, secret)))
    println("Quick encode:\t" + quickEncode(frenchCode)(decode(frenchCode, secret)))
    assert(quickEncode(frenchCode)(decode(frenchCode, secret)) === secret)
  }
}
