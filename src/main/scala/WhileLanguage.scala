import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.JavaTokenParsers

object WhileLanguage {

  var fact: String =
    """fact := 1 ;
      |val := 3;
      |cur := val ;
      |
      |while ( cur > 1 )
      |  do
      |   {
      |      fact := fact * cur ;
      |      cur := cur - 1
      |   }
      |""".stripMargin

  var fib: String =
    """a := 1;
      |b := 1;
      |n := 6;
      |
      |while ( n > 1 )
      |  do
      |   {
      |      fib := a + b ;
      |      a := b;
      |      b := fib;
      |      n := n - 1
      |   }
      |""".stripMargin

  var sort: String =
    """a := [4,3,2,1];
      |j:=0;
      |while(j<3)
      |do
      |{
      |   j:=j+1;
      |   i:=0;
      |   while(i<3)
      |   do
      |   {
      |     i:=i+1;
      |     if(a[i-1] > a[i]) then
      |     {
      |       t:=a[i-1];
      |       a[i-1]:=a[i];
      |       a[i]:=t
      |     }
      |   }
      |}
      |""".stripMargin

  val program: String = sort
  val wl = new WL()

  def main(args: Array[String]): Unit = {
    println(program)

    println(wl.parse(program).map { case (k, v) => s"$k $v" }.mkString("\n"))
  }

}

//noinspection RedundantBlock
class WL extends JavaTokenParsers {

  type State = collection.SortedMap[String, Store]
  type Transition = State => State

  class Store(store: mutable.ArrayBuffer[Long], isArray: Boolean, index: Int) {
    def value: Long = {
      if (isArray)
        throw new UnsupportedOperationException(s"$this is an array, value should not be called. " +
          s"Call element with an index which is in the range of (0,${store.size - 1})")
      store(index)
    }

    def element(state: State, idxExpr: State => Store): Long = store(idxExpr(state).value.toInt)

    def setElement(variables: State, idxExpr: State => Store, expr: State => Store): Unit = {
        val v = expr(variables).value
        store(idxExpr(variables).value.toInt) = v
    }

    override def toString: String = if (isArray) {
      store.mkString("[", ",", "]")
    } else {
      String.valueOf(value)
    }
  }

  object Store {
    implicit def apply(value: Long): Store = {
      val arr = new ArrayBuffer[Long](1)
      arr += value
      new Store(arr, isArray = false, index = 0)
    }
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
        val store = new Store(buff, isArray = true, index= -1)
        variables.updated(v, store)
  }

  def varRef: Parser[State => Store] = ident ^^ (v => { variables => variables(v) })

  def arrayElementInit: Parser[Transition] = ident ~ "[" ~ expr ~ "]" ~ ":=" ~ expr ^^ {
    case v ~ _ ~ idxExpr ~ _ ~ _ ~ expr => {
      variables =>
        variables(v).setElement(variables, idxExpr,expr)
        variables
    }
  }

  def arrayElementRef: Parser[State => Store] = ident ~ "[" ~ expr <~ "]" ^^ {
    case v ~ _ ~ idxExpr => {
      variables => variables(v).element(variables, idxExpr)
    }
  }

  def arrayLiteral: Parser[List[State => Store]] = "[" ~> repsep(expr, ",") <~ "]"

  def number: Parser[State => Store] = "\\d+".r ^^ (num => { _ => num.toLong })

  def factor: Parser[State => Store] = arrayElementRef | varRef | number | "(" ~> expr <~ ")"

  def term: Parser[State => Store] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => { variables => x(variables).value * y(variables).value }
      case (x, "/" ~ y) => { variables => x(variables).value / y(variables).value }
    }
  }

  def expr: Parser[State => Store] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
      case (x, "+" ~ y) => { variables => x(variables).value + y(variables).value }
      case (x, "-" ~ y) => { variables => x(variables).value - y(variables).value }
    }
  }


  def parse(source: String): State = parseAll(lines, source) match {
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
