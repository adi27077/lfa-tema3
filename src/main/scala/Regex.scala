import scala.annotation.tailrec

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  private def preprocess(s:List[Char]): List[Either[Char,Char]] = {

    @tailrec
    def helper(s: List[Char], acc: List[Either[Char, Char]]): List[Either[Char, Char]] = {
      s match {
        case Nil => acc
        case x :: xs => x match {
          case '[' =>
            val a = xs.head
            val b = xs.drop(2).head
            var l = List[Either[Char, Char]]()
            l = l :+ Right('(')
            for (i <- a to b) {
              if (i != b) l = l :+ Left(i) :+ Right('|')
              else l = l :+ Left(i)
            }
            l = l :+ Right(')')
            //println(xs, acc ++ l)
            helper(xs.drop(4), acc ++ l)
          case '*' => helper(xs, acc :+ Right('*'))
          case '+' =>
            val a = acc.last
            if (a == Right(')')) {
              val expr = Right('{') +: acc.drop(acc.lastIndexOf(Right('(')) + 1)
              val newAcc = (acc.take(acc.lastIndexOf(Right('('))) :+ Right('{')) ++ acc.drop(acc.lastIndexOf(Right('(')) + 1)
              //println(xs, newAcc ++ expr :+ Right('*'))
              helper(xs, newAcc ++ expr :+ Right('*'))
            } else {
            //println(xs, acc :+ Right('{') :+ a :+ Right('*'))
            helper(xs, acc.init :+ Right('{') :+ a :+ a :+ Right('*') :+ Right(')'))
            }
          case '?' =>
            val a = acc.last
            if (a == Right(')')) {
              val expr = acc.drop(acc.lastIndexOf(Right('(')))
              val rest = acc.take(acc.lastIndexOf(Right('('))) :+ Right('(')
              //println(xs, rest ++ expr :+ Right('|') :+ Left('ε') :+ Right(')'))
              helper(xs, rest ++ expr :+ Right('|') :+ Left('ε') :+ Right(')'))
            } else {
            //println(xs, acc :+ Right('(') :+ a :+ Right('|') :+ Left('ε') :+ Right(')'))
            helper(xs, acc.init :+ Right('(') :+ a :+ Right('|') :+ Left('ε') :+ Right(')'))
            }
          case '|' => helper(xs, acc :+ Right('|'))
          case '(' => helper(xs, acc :+ Right('('))
          case ')' =>
            //println(xs, (acc.take(acc.lastIndexOf(Right('('))) :+ Right('{')) ++ acc.drop(acc.lastIndexOf(Right('(')) + 1) :+ Right(')'))

            if (xs.nonEmpty && xs.head != '+') helper(xs, (acc.take(acc.lastIndexOf(Right('('))) :+ Right('{')) ++ acc.drop(acc.lastIndexOf(Right('(')) + 1) :+ Right(')'))
            else helper(xs, acc :+ Right(')'))
          case '\'' =>
            val a = xs.head
            if (a == '\\') {
              val b = xs.drop(1).head
              if (b == 'n') helper(xs.drop(3), acc :+ Left('\n'))
              else if (b == 't') helper(xs.drop(3), acc :+ Left('\t'))
              else if (b == 'r') helper(xs.drop(3), acc :+ Left('\r'))
              else if (b == 'f') helper(xs.drop(3), acc :+ Left('\f'))
              else if (b == 'b') helper(xs.drop(3), acc :+ Left('\b'))
              else if (b == '\'') helper(xs.drop(3), acc :+ Left('\''))
              else if (b == '\\') helper(xs.drop(3), acc :+ Left('\\'))
              else helper(xs.drop(3), acc :+ Left(b))
            }
            else if (a == 'a') helper(xs.drop(2), acc :+ Left('α'))
            else if (a == 'b') helper(xs.drop(2), acc :+ Left('β'))
            else if (a == 'c') helper(xs.drop(2), acc :+ Left('γ'))
            else if (a == 'l') helper(xs.drop(2), acc :+ Left('λ'))
            else helper(xs.drop(2), acc :+ Left(a))
          case 'e' =>
            if (xs.nonEmpty && xs.head == 'p' && xs.drop(1).head == 's') {
              helper(xs.drop(2), acc :+ Left('ε'))
            } else {
              helper(xs, acc :+ Left(x))
            }
          case _ => helper(xs, acc :+ Left(x))
        }
      }
    }

    helper(s,List())

  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    val s = str.toList
    val preprocessed = preprocess(s)
    val replacePara = preprocessed.map(x => if (x == Right('{')) Right('(') else x)

    def addConcatOperator(s: List[Either[Char, Char]]): List[Either[Char, Char]] = {
      @tailrec
      def helper(s: List[Either[Char, Char]], acc: List[Either[Char, Char]]): List[Either[Char, Char]] = {
        s match {
          case Nil => acc
          case x :: xs => x match {
            case Left(_) =>
              if (xs.nonEmpty) {
                if (xs.head.isLeft || xs.head == Right('(')) {
                  helper(xs, acc :+ x :+ Right('.'))
                } else {
                  helper(xs, acc :+ x)
                }
              } else {
                helper(xs, acc :+ x)
              }
            case Right(a) => a match {
              case '*' | ')' =>
                if (xs.nonEmpty) {
                  if (xs.head.isLeft || xs.head == Right('(')) {
                    helper(xs, acc :+ x :+ Right('.'))
                  } else {
                    helper(xs, acc :+ x)
                  }
                } else {
                  helper(xs, acc :+ x)
                }
              case '|' | '(' => helper(xs, acc :+ x)
              case _ => helper(xs, acc :+ x)
            }
          }
        }
      }

      helper(s, List())
    }

    //Transforms the regex from infix to prefix notation
    //using the shunting yard algorithm
    def infixToPrefix(s: List[Either[Char, Char]]): List[Either[Char, Char]] = {
      val precedence = Map(
        ')' -> 0,
        '(' -> 0,
        '|' -> 1,
        '.' -> 2,
        '*' -> 3
      )

      val s1 = (Right('(') :: s) :+ Right(')')
      var stack = List[Either[Char, Char]]()
      var output = List[Either[Char, Char]]()

      s1.foreach {
        case i@Left(_) => output = output :+ i
        case i@Right(x) => x match {
          case '(' => stack = stack :+ i
          case ')' =>
            while (stack.nonEmpty && stack.last != Right('(')) {
              output = output :+ stack.last
              stack = stack.dropRight(1)
            }
            stack = stack.dropRight(1)
          case '*' | '.' | '|' =>
            var cond = true
            while (stack.nonEmpty && cond) {
              if (precedence(stack.last.toOption.get) >= precedence(x)) {
                output = output :+ stack.last
                stack = stack.dropRight(1)
              } else {
                cond = false
              }
            }
            stack = stack :+ i
        }
      }

      while (stack.nonEmpty) {
        output = output :+ stack.last
        stack = stack.dropRight(1)
      }

      output.reverse
    }

    //println(replacePara)
    val withConcat = addConcatOperator(replacePara)
    //println(withConcat)
    //Reverse the regex and replace '(' with ')' and vice versa
    val withConcatReversed = withConcat.reverse.map {
      case Right('(') => Right(')')
      case Right(')') => Right('(')
      case x => x
    }

    val output = infixToPrefix(withConcatReversed)
    //println(output)
    var prenex = ""

    output.foreach {
      case Left(x) =>
        if (x == 'ε') prenex += "eps "
        else if (x == 'α') prenex += "'a' "
        else if (x == 'β') prenex += "'b' "
        else if (x == 'γ') prenex += "'c' "
        else if (x == 'λ') prenex += "'l' "
        else if (x == '\t') prenex += "\t " //Test escaped chars 2 ????????
        else if (x == '\n') prenex += "\n " //Test escaped chars 2 ????????
        else if (" \n\t\'|*+-?()\\.".contains(x)) prenex += "'" + x + "' "
        else prenex += x + " "
      case Right(x) =>
        x match {
          case '*' => prenex += "STAR "
          case '|' => prenex += "UNION "
          case '.' => prenex += "CONCAT "
          case '(' => return prenex
          case ')' => return prenex
        }
    }

    //println(prenex)
    prenex
  }
}
