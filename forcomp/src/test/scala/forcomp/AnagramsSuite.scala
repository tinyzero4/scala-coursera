package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    println(combinations(abba).toSet mkString "\n")
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("test collection methods") {
    val x1 = List(1, 2, 3, 45)
    val s = "Hello World"
    println((x1 zip s) map (p => p._1))

    println(isPrime(3))
    println(isPrime(17))
    println(isPrime(128))
    println(isSumPrime(10))
    println(List(1, 2, 3).flatMap(x => List(x * 2)))

    val map1 = Map[String, Int]("str" -> 1) withDefaultValue 0
    val map2 = Map[String, Int]("str" -> 2, "new" -> 222) withDefaultValue 0
    println(map1.foldLeft(map2)((accum: Map[String, Int], p: (String, Int)) =>
      accum + (p._1 -> (accum(p._1) + p._2))
    ))

    val res = for (x <- map1) yield x
    println(res)


    val smap = Map[Char, Int]('S' -> 1, 'B' -> 2, 'C' -> 3) withDefaultValue 0
    println("Hello Scala BC" map smap mkString)

  }

  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)

  def isSumPrime(bounds: Int): Seq[(Int, Int)] = {
    for {
      i <- 1 until bounds
      j <- 1 until bounds
      if isPrime(i + j)
    } yield (i, j)
  }

  test("Tests") {
    val x = List('a' -> 1, 'b' -> 1, 'c' -> 1)
    x.foldLeft(List[(Char,Int)]())( (r, e) => e :: r)

  }

}
