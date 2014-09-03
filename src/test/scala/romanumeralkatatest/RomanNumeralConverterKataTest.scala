package romanumeralkatatest

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks
import romannumeralkata.RomanNumeralConverter

class RomanNumeralConverterKataTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {

  val numerals = Table(
    ("arabic",  "roman"),
    (1,         "I"),
    (2,         "II"),
    (3,         "III"),
    (4,         "IV"),
    (5,         "V"),
    (6,         "VI"),
    (7,         "VII"),
    (8,         "VIII"),
    (9,         "IX"),
    (10,        "X")
  )

  forAll (numerals) { (arabic: Int, roman: String) => {
    RomanNumeralConverter.arabicToString(arabic) should be === roman
  }}
}
