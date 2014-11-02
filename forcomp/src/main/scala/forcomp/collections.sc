import scala.io.Source
object collections {
  /* read a file of words */
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
  /* create a list and filter all words where *all* their characters are not letters (like dashes) */
  val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))
  /* define the map of numbers to letters */
  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
  /* invert the map the get a map of letters to digits */
  val charCode: Map[Char, Char] =
    for ( (digit, str) <- mnem; char <- str) yield char -> digit
  def wordCode(word:String):String = word.toUpperCase map charCode
  wordCode("Java")
  val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List())
    else {
      for (
        split <- 1 to number.length;
        word <- wordsForNum( number take split);
        rest <- encode(number drop split)
      ) yield word :: rest                                                                                 cle
    }.toSet
  }

  encode("7225247386")
  def translate(number: String): Set[String] =
    encode(number) map (_ mkString " ")

  translate("7225247386")
}

