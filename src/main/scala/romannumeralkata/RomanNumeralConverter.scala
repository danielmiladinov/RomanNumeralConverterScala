package romannumeralkata


object RomanNumeralConverter {

  private val ones = new OneFivesAndTensSymbolPattern(1, "I", "V", "X")
  private val tens = new OneFivesAndTensSymbolPattern(10, "X", "L", "C")
  private val hundreds = new OneFivesAndTensSymbolPattern(100, "C", "D", "M")
  private val thousands = new OnesSymbolPattern(1000, "M")

  def arabicToString (arabic: Int): String = {
    List(thousands, hundreds, tens, ones) map { _.getSymbol(arabic) } mkString ""
  }

  private trait SymbolPattern {
    val magnitude: Int
    val symbolTable: Map[Int, String]

    def getSymbol (arabic: Int): String = {
      val value = if (arabic >= magnitude)
        arabic % (magnitude * 10) / magnitude
      else
        0
      symbolTable getOrElse(value, "")
    }
  }

  private class OnesSymbolPattern (val magnitude: Int, o: String) extends SymbolPattern {
    val symbolTable = Map(
      1  -> s"$o",
      2  -> s"$o$o",
      3  -> s"$o$o$o"
    )
  }

  private class OneFivesAndTensSymbolPattern (val magnitude: Int, o: String, f: String, t: String) extends SymbolPattern {
    val symbolTable = Map(
      1  -> s"$o",
      2  -> s"$o$o",
      3  -> s"$o$o$o",
      4  -> s"$o$f",
      5  -> s"$f",
      6  -> s"$f$o",
      7  -> s"$f$o$o",
      8  -> s"$f$o$o$o",
      9  -> s"$o$t"
    )
  }
}
