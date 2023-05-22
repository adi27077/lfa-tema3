import scala.annotation.tailrec

class Nfa[A](val startState: A, val transitions: Map[(A, Char), Set[A]], val finalStates: Set[A], val states: Set[A],
             val tokenMap: Map[String, Set[A]]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = {
    val newStates = states.map(f)
    val newTransitions = transitions.map({case ((a, c), b) => (f(a), c) -> b.map(f)})
    val newFinalStates = finalStates.map(f)
    val newTokenMap = tokenMap.map({case (s, a) => s -> a.map(f)})
    new Nfa(f(startState), newTransitions, newFinalStates, newStates, newTokenMap)
  } // TODO implement map

  def next(state:A, c: Char): Set[A] = {
    var nextStates = transitions.getOrElse((state, c), Set())
    val epsilonStates = transitions.getOrElse((state, 'ε'), Set())

    epsilonStates.filterNot(_ == state).foreach(s => nextStates ++= next(s, c))

    nextStates
  } // TODO implement next

  def accepts(str: String): Boolean = {

    @tailrec
    def acceptsRec(states: Set[A], str: String): Boolean = {
      if (str.isEmpty) {
        if (states.intersect(finalStates).isEmpty) {
          val epsilonStates = states.flatMap(s => next(s, 'ε'))
          epsilonStates.intersect(finalStates).nonEmpty
        } else {
          true
        }
      } else {
        val nextStates = states.flatMap(s => next(s, str.head))
        acceptsRec(nextStates, str.tail)
      }
    }

    acceptsRec(Set(startState), str)

  } // TODO implement accepts

  def getStartState: A = {
    startState
  }

  def getTransitions: Map[(A, Char), Set[A]] = {
    transitions
  }

  def getFinalStates: Set[A] = {
    finalStates
  }

  def getStates : Set[A] = {
    states
  } // TODO implement getStates

  def getTokenMap: Map[String, Set[A]] = {
    tokenMap
  }

  def isFinal(state: A): Boolean = {
    finalStates.contains(state)
  }  // TODO implement isFinal

  def print(): Unit = {
    println("Start state: " + startState)
    println("Transitions: " + transitions)
    println("Final states: " + finalStates)
    println("States: " + states)
    println("Token map: " + tokenMap)
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {
  class BTree[A](var value: A, var left: Option[BTree[A]], var right: Option[BTree[A]]) {
    def map[B](f: A => B): BTree[B] = {
      new BTree(f(value), left.map(_.map(f)), right.map(_.map(f)))
    }

    def addLeft(leftTree: BTree[A]): Unit = {
      left = Some(leftTree)
    }

    def addRight(rightTree: BTree[A]): Unit = {
      right = Some(rightTree)
    }

    def setValue(a: A): Unit = {
      value = a
    }

    def getLeft: Option[BTree[A]] = {
      left
    }

    def getRight: Option[BTree[A]] = {
      right
    }

    def getValue: A = {
      value
    }

    override def toString: String = {
      val leftStr = left.map(_.toString).getOrElse("")
      val rightStr = right.map(_.toString).getOrElse("")
      s"($leftStr$value$rightStr)"
    }
  }

  def fromPrenex(str: String, token: String): Nfa[Int] = {
    val tokens = str.split(" (?=(?:[^']*'[^']*')*[^']*$)").toList
    val operators2 = Set("CONCAT", "UNION")
    val operators1 = Set("STAR", "PLUS", "MAYBE")


    def fromPrenexRec(tokens: List[String], tree: BTree[String]): (BTree[String], List[String]) = {
      tokens match {
        case Nil => (tree, Nil)
        case token :: rest =>
          if (operators2.contains(token)) {
            tree.setValue(token)
            val leftTree = new BTree("", None, None)
            val rightTree = new BTree("", None, None)
            val (returnedTree, tokensLeft) = fromPrenexRec(rest, leftTree)
            tree.addLeft(returnedTree)
            val (returnedTree2, tokensLeft2) = fromPrenexRec(tokensLeft, rightTree)
            tree.addRight(returnedTree2)
            (tree, tokensLeft2)
          } else if (operators1.contains(token)) {
            tree.setValue(token)
            val leftTree = new BTree("", None, None)
            val (returnedTree, tokensLeft) = fromPrenexRec(rest, leftTree)
            tree.addLeft(returnedTree)
            (tree, tokensLeft)
          } else {
            tree.setValue(token)
            (tree, rest)
          }
      }

    }

    def fromTreeToNfa(tree: BTree[String]): Nfa[Int] = {
      val states = collection.mutable.Set[Int]()
      val transitions = collection.mutable.Map[(Int, Char), Set[Int]]()
      val finalStates = collection.mutable.Set[Int]()
      var stateCounter = -1

      def fromTreeToNfaRec(tree: BTree[String]): Unit = {
        def addEpsilonTransition(from: Int, to: Int): Unit = {
          if (transitions.contains((from, 'ε'))) {
            transitions((from, 'ε')) += to
          } else {
            transitions += ((from, 'ε') -> Set(to))
          }
        }

        def addTransition(from: Int, to: Int, c: Char): Unit = {
          if (transitions.contains((from, c))) {
            transitions((from, c)) += to
          } else {
            transitions += ((from, c) -> Set(to))
          }
        }

        tree.getValue match {
          case "CONCAT" =>
            fromTreeToNfaRec(tree.getLeft.get)
            addEpsilonTransition(stateCounter, stateCounter + 1)
            fromTreeToNfaRec(tree.getRight.get)
          case "UNION" =>
            stateCounter += 1
            states += stateCounter
            addEpsilonTransition(stateCounter, stateCounter + 1)
            val leftState = stateCounter
            fromTreeToNfaRec(tree.getLeft.get)
            addEpsilonTransition(leftState, stateCounter + 1)
            val leftExitState = stateCounter
            fromTreeToNfaRec(tree.getRight.get)
            addEpsilonTransition(leftExitState, stateCounter + 1)
            addEpsilonTransition(stateCounter, stateCounter + 1)
            stateCounter += 1
            states += stateCounter
          case "STAR" =>
            stateCounter += 1
            states += stateCounter
            val leftState = stateCounter
            addEpsilonTransition(stateCounter, stateCounter + 1)
            fromTreeToNfaRec(tree.getLeft.get)
            addEpsilonTransition(stateCounter, leftState + 1)
            addEpsilonTransition(leftState, stateCounter + 1)
            addEpsilonTransition(stateCounter, stateCounter + 1)
            stateCounter += 1
            states += stateCounter
          case "PLUS" =>
            stateCounter += 1
            states += stateCounter
            val leftState = stateCounter
            addEpsilonTransition(stateCounter, stateCounter + 1)
            fromTreeToNfaRec(tree.getLeft.get)
            addEpsilonTransition(stateCounter, leftState + 1)
            addEpsilonTransition(stateCounter, stateCounter + 1)
            stateCounter += 1
            states += stateCounter
          case "MAYBE" =>
            stateCounter += 1
            states += stateCounter
            val leftState = stateCounter
            addEpsilonTransition(stateCounter, stateCounter + 1)
            fromTreeToNfaRec(tree.getLeft.get)
            addEpsilonTransition(leftState, stateCounter + 1)
            addEpsilonTransition(stateCounter, stateCounter + 1)
            stateCounter += 1
            states += stateCounter
          case default =>
            if (default == "void")
              return
            stateCounter += 1
            states += stateCounter
            val character = if (default == "eps") {
              'ε'
            } else {
              if (default.matches("'.'")) {
                default.charAt(1)
              } else {
                default.charAt(0)
              }
            }
            addTransition(stateCounter, stateCounter + 1, character)
            stateCounter += 1
            states += stateCounter
        }
      }

      fromTreeToNfaRec(tree)
      if (stateCounter >= 0) {
        finalStates += stateCounter
      }

      new Nfa(0, transitions.toMap, finalStates.toSet, states.toSet, Map(token -> finalStates.toSet))
    }

    val (syntaxTree, _) = fromPrenexRec(tokens, new BTree("", None, None))

    fromTreeToNfa(syntaxTree)
  } // TODO implement Prenex -> Nfa transformation.

  // You can add more methods to this object
}
