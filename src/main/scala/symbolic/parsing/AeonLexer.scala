package symbolic.parsing

import symbolic.parsing.AeonTokens.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object AeonLexer extends RegexParsers {
  override def skipWhitespace = true

  override val whiteSpace: Regex = "[ \t\r\f\n]+".r

  def apply(aeon: String): List[AeonToken] = parse(tokens, aeon) match {
    case Success(result, next) => result
    case NoSuccess(msg, next) => throw RuntimeException(s"lexer error: $msg")
    case _ => throw RuntimeException("unknown lexer exception")
  }

  private def tokens: Parser[List[AeonToken]] = {
    phrase(rep1(
      id | lparen | rparen | comma | not | or | and | xor | imp | biimp
    ))
  }

  private def id: Parser[ID] = """[a-zA-Z0-9]+[_a-zA-Z0-9+]*""".r ^^ { s => ID(s) }

  private def lparen: Parser[LPAREN] = "(" ^^ { _ => LPAREN() }

  private def rparen: Parser[RPAREN] = ")" ^^ { _ => RPAREN() }

  private def comma: Parser[COMMA] = "," ^^ { _ => COMMA() }

  private def not: Parser[NOT] = "!" ^^ { _ => NOT() }

  private def or: Parser[OR] = "|" ^^ { _ => OR() }

  private def and: Parser[AND] = "&" ^^ { _ => AND() }

  private def xor: Parser[XOR] = "^" ^^ { _ => XOR() }

  private def imp: Parser[IMP] = "=>" ^^ { _ => IMP() }

  private def biimp: Parser[BIIMP] = "<=>" ^^ { _ => BIIMP() }
}
