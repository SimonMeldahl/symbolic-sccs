package symbolic.parsing

object AeonTokens {
  sealed trait AeonToken

  case class ID(s: String) extends AeonToken

  case class LPAREN() extends AeonToken

  case class RPAREN() extends AeonToken

  case class COMMA() extends AeonToken

  case class NOT() extends AeonToken

  case class OR() extends AeonToken

  case class AND() extends AeonToken

  case class XOR() extends AeonToken

  case class IMP() extends AeonToken

  case class BIIMP() extends AeonToken
}
