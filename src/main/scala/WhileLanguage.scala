import java.io.FileReader
import scala.collection.mutable
import scala.io.Source
import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.StreamReader

object WhileLanguage {

  def main(args: Array[String]): Unit = {
    val wl = new WhileLanguageParser()
    val file = args.headOption.getOrElse("src/main/WhileLanguage/sort.txt")
    val sourceCode = Source.fromFile(file)
    println(sourceCode.mkString)
    val variables = wl.executeFile(file)
    println(variables.map { case (k, v) => s"$k: $v" }.mkString("\n"))
  }

}

//noinspection RedundantBlock
class WhileLanguageParser extends JavaTokenParsers {

  type State = collection.SortedMap[String, Store]
  type Transition = State => State

  trait Store

  class Value(val value: Long) extends Store {
    override def toString: String = String.valueOf(value)
  }

  implicit def longToValue(value: Long): Value = new Value(value)

  class Array(store: mutable.ArrayBuffer[Long]) extends Store {
    def element(state: State, idxExpr: State => Value): Long = store(idxExpr(state).value.toInt)

    def setElement(variables: State, idxExpr: State => Value, expr: State => Value): Unit = {
      val v = expr(variables).value
      store(idxExpr(variables).value.toInt) = v
    }

    override def toString: String = store.mkString("[", ",", "]")
  }

  def lines: Parser[List[Transition]] = repsep(varInit | arrayVarInit | arrayElementInit | while_ | ifElse | ifThen, ";")

  def while_ : Parser[Transition] =
    ("while" ~> "(" ~> "[^)]+".r <~ ")" ~ "do" ~ "{") ~ lines <~ "}" ^^ {

      case bool ~ parsedLines =>

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
    ("if" ~> "(" ~> "[^)]+".r <~ ")" <~ "then" <~
      "{") ~ (lines <~ "}" <~
      "else" <~
      "{") ~ (lines <~ "}") ^^ {

      case bool ~ parsedIfLines ~ parsedElseLines =>

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
    ("if" ~> "(" ~> "[^)]+".r <~ ")" <~ "then" <~
      "{") ~ (lines <~ "}") ^^ {

      case bool ~ parsedLines =>

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
    case a ~ ">" ~ b => { variables => a(variables).value > b(variables).value }
  }

  def lt: Parser[State => Boolean] = expr ~ "<" ~ expr ^^ {
    case a ~ "<" ~ b => { variables => a(variables).value < b(variables).value }
  }

  def varInit: Parser[Transition] = ident ~ ":=" ~ expr ^^ {
    case v ~ _ ~ expr =>
      variables => variables.updated(v, expr(variables))
  }

  def arrayVarInit: Parser[Transition] = ident ~ ":=" ~ arrayLiteral ^^ {
    case v ~ _ ~ list =>
      variables =>
        val buff = list.foldLeft(mutable.ArrayBuffer[Long]()) {
          case (b, expr) =>
            b += expr(variables).value
            b
        }
        variables.updated(v, new Array(buff))
  }

  def varRef: Parser[State => Value] = ident ^^ (v => { variables => variables(v).asInstanceOf[Value] })

  def arrayElementInit: Parser[Transition] = ident ~ "[" ~ expr ~ "]" ~ ":=" ~ expr ^^ {
    case v ~ _ ~ idxExpr ~ _ ~ _ ~ expr => {
      variables =>
        variables(v).asInstanceOf[Array].setElement(variables, idxExpr, expr)
        variables
    }
  }

  def arrayElementRef: Parser[State => Value] = ident ~ "[" ~ expr <~ "]" ^^ {
    case v ~ _ ~ idxExpr => {
      variables => variables(v).asInstanceOf[Array].element(variables, idxExpr)
    }
  }

  def arrayLiteral: Parser[List[State => Value]] = "[" ~> repsep(expr, ",") <~ "]"

  def number: Parser[State => Value] = "\\d+".r ^^ (num => { _ => num.toLong })

  def factor: Parser[State => Value] = arrayElementRef | varRef | number | "(" ~> expr <~ ")"

  def term: Parser[State => Value] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "*" ~ y) => { variables => x(variables).value * y(variables).value }
      case (x, "/" ~ y) => { variables => x(variables).value / y(variables).value }
    }
  }

  def expr: Parser[State => Value] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "+" ~ y) => { variables => x(variables).value + y(variables).value }
      case (x, "-" ~ y) => { variables => x(variables).value - y(variables).value }
    }
  }


  def executeString(sourceCode: String): State = state(parseAll(lines, sourceCode))

  def executeFile(file: String): State = {
    val sourceCode = StreamReader(new FileReader(file))
    state(parseAll(lines, sourceCode)
    )
  }

  def state(result: ParseResult[List[Transition]]): State = result match {
    case Success(expression, _) =>
      expression.foldLeft(collection.SortedMap(): State) {
        case (vs, t) => t(vs)
      }

    case NoSuccess(err, next) =>
      println(err)
      throw new IllegalArgumentException("failed to parse " +
        "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
        err + "\n" + next.pos.longString)
  }

  def parseBool(sourceCode: String, variables: State): Boolean = parseAll(boolExpr, sourceCode) match {
    case Success(expression, _) =>
      expression(variables)

    case NoSuccess(err, next) =>
      println(err)
      throw new IllegalArgumentException("failed to parse " +
        "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
        err + "\n" + next.pos.longString)
  }
}
