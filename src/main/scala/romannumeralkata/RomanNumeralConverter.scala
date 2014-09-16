package romannumeralkata


object RomanNumeralConverter {

  private val toArabic = Map(
    'I' -> 1,
    'V' -> 5,
    'X' -> 10,
    'L' -> 50,
    'C' -> 100,
    'D' -> 500,
    'M' -> 1000
  )

  def romanStringToArabic (roman: String): Int = {
    // Work begins with a sum of zero, and no previous character
    val initial: (Int, Option[Char]) = (0, None)

    roman.foldLeft (initial) ((acc: (Int, Option[Char]), curr: Char) => {
      val currVal = toArabic(curr)

      acc match {
        // Starting case - just accept the current value and letter
        // as the sum and previous character for the next round
        case (_, None) => (currVal, Some(curr))

        case (sum, Some(prev)) => {
          val prevVal = toArabic(prev)

          // The next value is either an addition or a subtraction, depending on whether
          // the current value is greater than the previous value or not
          val next = if (currVal > prevVal) {
            // When current is greater than previous, invoke subtractive principle
            // The previous value is subtracted from the current, plus we remove its
            // value from the sum again since it can't be an addition
            currVal - (prevVal * 2)
          } else {
            // Otherwise, it's just a simple addition
            currVal
          }

          // The item of work for the next round is the sum + next and the current character
          (sum + next, Some(curr))
        }
      }
    })._1 // When the work is done we take the sum, the first element of the tuple
  }

  private val ones = new OneFivesAndTensSymbolPattern(1, "I", "V", "X")
  private val tens = new OneFivesAndTensSymbolPattern(10, "X", "L", "C")
  private val hundreds = new OneFivesAndTensSymbolPattern(100, "C", "D", "M")
  private val thousands = new OnesSymbolPattern(1000, "M")

  def arabicToRomanString (arabic: Int): String = {
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
