package symbolic.parsing

import symbolic.{BDD, BDDVar, BooleanNetwork}
import symbolic.BDD.*
import symbolic.parsing.AeonLexer.apply
import symbolic.parsing.AeonTokens.*

import scala.util.parsing.combinator.*
import scala.util.parsing.input.*

private class AeonParser(using varsToBdd: Map[String, BDDVar], funsToBdds: Map[String, List[BDDVar]]) extends Parsers {
  override type Elem = AeonToken

  class AeonTokenReader(tokens: Seq[AeonToken]) extends Reader[AeonToken] {
    override def first: AeonToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = NoPosition

    override def rest: Reader[AeonToken] = new AeonTokenReader(tokens.tail)
  }

  def parse(aeon: String): BDD = {
    val tokens = AeonLexer(aeon)
    val reader = new AeonTokenReader(tokens)
    createBdd(reader) match {
      case NoSuccess(msg, next) => throw RuntimeException(s"parser error $msg")
      case Success(result, next) => result
      case _ => throw RuntimeException(s"some unknown parser error, whoops")
    }
  }

  def createBdd: Parser[BDD] = exp()

  def exp(level: Int = 3): Parser[BDD] = level match {
    case 3 => binop | exp(2)
    case 2 => not | exp(1)
    case 1 => funcall | exp(0)
    case 0 => id | parens
  }

  def identifier: Parser[ID] = {
    accept("id", { case id@ID(name) => id })
  }

  def id: Parser[BDD] = identifier - parens ^^ { case ID(s) => s match {
    case "false" => FALSE
    case "true" => TRUE
    case _ => varsToBdd(s)
  }
  }

  def parens: Parser[BDD] = (LPAREN() ~ exp() ~ RPAREN()) ^^ { case _ ~ e ~ _ => e }

  def not: Parser[BDD] = (NOT() ~ exp(1)) ^^ { case _ ~ e => !e }

  def binop: Parser[BDD] = {
    exp(2) * {
      (OR() | AND() | XOR() | IMP() | BIIMP()) ^^ {
        op => { (e1: BDD, e2: BDD) =>
          op match {
            case OR() => e1 \/ e2
            case AND() => e1 /\ e2
            case XOR() => xor(e1, e2)
            case IMP() => e1 ==> e2
            case BIIMP() => e1 <==> e2
            case _ => throw RuntimeException(s"unexpected op $op")
          }
        }
      }
    }
  }

  def funcall: Parser[BDD] = (identifier ~ LPAREN() ~ repsep(identifier, COMMA()) ~ RPAREN()) ^^ { case ID(f) ~ _ ~ args ~ _ =>
    BooleanNetwork.expandFunctionCall(s"$f${args.map { case ID(a) => a }.mkString("(", ", ", ")")}")
  }
}

object AeonParser {
  def apply(aeon: String)(using varsToBdd: Map[String, BDDVar], funsToBdds: Map[String, List[BDDVar]]): BDD = {
    new AeonParser().parse(aeon)
  }
}

