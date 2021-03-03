package interpreter

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers

object WhileLanguage {

  var act: String =
    """f:=1;
      |if (f<3) then {
      |a:=2
      |} else {
      |b:=3
      |}
      |""".stripMargin

  var fact: String =
    """fact := 1 ;
      |val := 5;
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

  var nestedProgram: String =
    """x := 2 ;
      |y := 3;
      |prod:=1;
      |
      |while ( x > 1 ) do
      |{
      |   while ( y > 1 ) do
      |   {
      |      prod := prod * x * y ;
      |      y := y - 1
      |   } ;
      |   x := x - 1
      |}
      |""".stripMargin

  val wl = new WL()

  def main(args: Array[String]): Unit = {
    val program = fact
    program.linesWithSeparators.zipWithIndex.foreach {
      case (line, index) => print(s"${index + 1}: $line")
    }

    println
    println("result:")
    println(wl.run(program).toList.sorted.map { case (k, v) => s"$k = $v" }.mkString("\n"))
  }

  //noinspection RedundantBlock
  class WL extends JavaTokenParsers {

    val variables = mutable.Map.empty[String, Long]

    def lines: Parser[Any] = repsep(varInit | while_ | ifElse | ifThen, ";")

    def while_ : Parser[Any] =
      ("while" ~> "(" ~> "[^)]+".r <~ ")" ~ "do" ~ "{").into { bool => { in =>
        while (isTrue(bool)) {
          guard(lines <~ "}")(in)
        }
        ("[^}]*".r ~ "}") (in)
      }
      }

    def ifElse: Parser[Any] =
      ("if" ~> "(" ~> "[^)]+".r <~ ")" <~ "then" <~ "{").into { bool =>
        if (isTrue(bool))
          lines <~ "}" <~
            "else" <~
            "{" <~ "[^}]*".r <~ "}"
        else
          "[^}]*".r ~> "}" ~>
            "else" ~>
            "{" ~> (lines <~ "}")
      }

    def ifThen: Parser[Any] =
      ("if" ~> "(" ~> "[^)]+".r <~ ")" <~ "then" <~ "{").into { bool =>
        if (isTrue(bool))
          lines <~ "}"
        else
          "[^}]*".r ~> "}"
      }

    def boolExpr: Parser[Boolean] = boolTerm ~ rep("and" ~ boolTerm | "or" ~ boolTerm) ^^ {
      case number ~ list => list.foldLeft(number) {
        case (x, "and" ~ y) => {
          x && y
        }
        case (x, "or" ~ y) => {
          x || y
        }
      }
    }

    def boolTerm: Parser[Boolean] = gt | lt

    def gt: Parser[Boolean] = expr ~ ">" ~ expr ^^ {
      case a ~ ">" ~ b => {
        a > b
      }
    }

    def lt: Parser[Boolean] = expr ~ "<" ~ expr ^^ {
      case a ~ "<" ~ b => {
        a < b
      }
    }

    def varInit: Parser[Any] = ident ~ ":=" ~ expr ^^ {
      case v ~ _ ~ expr =>
        List(
          variables(v) = expr
        )
    }

    def varRef: Parser[Long] = ident ^^ (v => {
      variables(v)
    })

    def number: Parser[Long] = "\\d+".r ^^ (num => num.toLong)

    def factor: Parser[Long] = varRef | number | "(" ~> expr <~ ")"

    def term: Parser[Long] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
      case number ~ list => (number /: list) {
        case (x, "*" ~ y) => {
          x * y
        }
        case (x, "/" ~ y) => {
          x / y
        }
      }
    }

    def expr: Parser[Long] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
      case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
        case (x, "+" ~ y) => {
          x + y
        }
        case (x, "-" ~ y) => {
          x - y
        }
      }
    }


    def run(source: String): mutable.Map[String, Long] = parseAll(lines, source) match {
      case Success(_, _) =>
        variables

      case NoSuccess(err, next) =>
        println(err)
        throw new IllegalArgumentException("failed to parse " +
          "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
          err + "\n" + next.pos.longString)
    }

    def isTrue(source: String): Boolean = parseAll(boolExpr, source) match {
      case Success(expression, _) =>
        expression

      case NoSuccess(err, next) =>
        println(err)
        throw new IllegalArgumentException("failed to parse " +
          "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
          err + "\n" + next.pos.longString)
    }
  }

}
