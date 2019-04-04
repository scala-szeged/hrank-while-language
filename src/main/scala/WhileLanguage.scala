package functional.programming.InterpreterAndCompilers

import scala.util.parsing.combinator.JavaTokenParsers

object WhileLanguage {

  var program: String =
    """fact := 1 ;
      |val := 10000 ;
      |cur := val ;
      |mod := 1000000007 ;
      |
      |while ( cur > 1 )
      |  do
      |   {
      |      fact := fact * cur ;
      |      fact := fact - fact / mod * mod ;
      |      cur := cur - 1
      |   } ;
      |
      |cur := 0""".stripMargin

  val wl = new WL()

  def main(args: Array[String]): Unit = {

    val bytes = new Array[Byte](1024000)
    val len = System.in.read(bytes)
    program = bytes.take(len).map(_.toChar).mkString
    println(program)


    println(wl.parse(program).toList.sorted.map { case (k, v) => s"$k $v" }.mkString("\n"))
  }

}

//noinspection RedundantBlock
class WL extends JavaTokenParsers {

  type State = Map[String, Long]
  type Transition = State => State


  def lines: Parser[List[Transition]] = repsep(varInit | while_ | ifElse | ifThen, ";")

  def while_ : Parser[Transition] =
    "while" ~> "(" ~> "[^)]+".r ~ ")" ~ "do" ~ "{" ~ lines ~ "}" ^^ {

      case bool ~ _ ~ _ ~ _ ~ parsedLines ~ _ =>

        variables => while_loop(variables, bool, parsedLines)
    }

  def while_loop(variables: State, bool: String, parsedLines: List[Transition]): State = {
    if (parseBool(bool, variables)) {
      while_loop(
        parsedLines.foldLeft(variables) {
          case (vs, t) => t(vs)
        },
        bool,
        parsedLines
      )
    } else {
      variables
    }
  }

  def ifElse: Parser[Transition] =
    "if" ~> "(" ~> "[^)]+".r ~ ")" ~ "then" ~
      "{" ~ lines ~ "}" ~
      "else" ~
      "{" ~ lines ~ "}" ^^ {

      case bool ~ _ ~ _ ~ _ ~ parsedIfLines ~ _ ~
        _ ~ _ ~ parsedElseLines ~ _ =>

        variables =>
          if (parseBool(bool, variables)) {
            parsedIfLines.foldLeft(variables) {
              case (vs, t) => t(vs)
            }
          } else {
            parsedElseLines.foldLeft(variables) {
              case (vs, t) => t(vs)
            }
          }
    }

  def ifThen: Parser[Transition] =
    "if" ~> "(" ~> "[^)]+".r ~ ")" ~ "then" ~
      "{" ~ lines ~ "}" ^^ {

      case bool ~ _ ~ _ ~ _ ~ parsedLines ~ _ =>

        variables =>
          if (parseBool(bool, variables)) {
            parsedLines.foldLeft(variables) {
              case (vs, t) => t(vs)
            }
          } else {
            variables
          }
    }

  def boolExpr: Parser[State => Boolean] = boolTerm ~ rep("and" ~ boolTerm | "or" ~ boolTerm) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "and" ~ y) => { variables => x(variables) && y(variables) }
      case (x, "or" ~ y) => { variables => x(variables) || y(variables) }
    }
  }

  def boolTerm: Parser[State => Boolean] = gt | lt

  def gt: Parser[State => Boolean] = expr ~ ">" ~ expr ^^ {
    case a ~ ">" ~ b => { variables => a(variables) > b(variables) }
  }

  def lt: Parser[State => Boolean] = expr ~ "<" ~ expr ^^ {
    case a ~ "<" ~ b => { variables => a(variables) < b(variables) }
  }

  def varInit: Parser[Transition] = ident ~ ":=" ~ expr ^^ {
    case v ~ _ ~ expr =>
      variables => variables.updated(v, expr(variables))
  }

  def varRef: Parser[State => Long] = ident ^^ (v => { variables => variables(v) })

  def number: Parser[State => Long] = "\\d+".r ^^ (num => { _ => num.toInt })

  def factor: Parser[State => Long] = varRef | number | "(" ~> expr <~ ")"

  def term: Parser[State => Long] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => { variables => x(variables) * y(variables) }
      case (x, "/" ~ y) => { variables => x(variables) / y(variables) }
    }
  }

  def expr: Parser[State => Long] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
      case (x, "+" ~ y) => { variables => x(variables) + y(variables) }
      case (x, "-" ~ y) => { variables => x(variables) - y(variables) }
    }
  }


  def parse(source: String): Map[String, Long] = parseAll(lines, source) match {
    case Success(expression, _) =>
      expression.foldLeft(Map(): State) {
        case (vs, t) => t(vs)
      }

    case NoSuccess(err, next) =>
      println(err)
      throw new IllegalArgumentException("failed to parse " +
        "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
        err + "\n" + next.pos.longString)
  }

  def parseBool(source: String, variables: State): Boolean = parseAll(boolExpr, source) match {
    case Success(expression, _) =>
      expression(variables)

    case NoSuccess(err, next) =>
      println(err)
      throw new IllegalArgumentException("failed to parse " +
        "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
        err + "\n" + next.pos.longString)
  }
}
