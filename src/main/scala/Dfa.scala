class Dfa[A] (val startState: A, val sink: A, val transitions: Map[(A, Char), A], val finalStates: Set[A], val states: Set[A],
              val tokenMap: Map[String, Set[A]]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = {
    val newStates = states.map(f)
    val newTransitions = transitions.map({case ((a, c), b) => (f(a), c) -> f(b)})
    val newFinalStates = finalStates.map(f)
    val newTokenMap = tokenMap.map({case (s, a) => s -> a.map(f)})
    new Dfa(f(startState), f(sink), newTransitions, newFinalStates, newStates, newTokenMap)
  } // TODO implement map

  def next(state:A, c: Char): A = {
    transitions.getOrElse((state, c), sink)
  } // TODO implement next

  def accepts(str: String): Boolean = {
    var state = startState
    str.foreach(c => state = next(state, c))
    finalStates.contains(state)
  } // TODO implement accepts

  def getStates : Set[A] = {
    states
  } // TODO implement getStates

  def isFinal(state: A): Boolean = {
    finalStates.contains(state)
  }  // TODO implement isFinal

  def getStartState: A = {
    startState
  }

  def getTransitions: Map[(A, Char), A] = {
    transitions
  }

  def getTokenMap: Map[String, Set[A]] = {
    tokenMap
  }

  def print(): Unit = {
    println("Start state: " + startState)
    println("Sink state: " + sink)
    println("Final states: " + finalStates)
    println("States: " + states)
    println("Transitions: " + transitions)
    println("Token map: " + tokenMap)
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String, token: String): Dfa[Int] = {
    if (str == "void") {
      return new Dfa(0, -1, Map(), Set(), Set(), Map())
    }

    val nfa = Nfa.fromPrenex(str, token)

    nfaToDfa(nfa)
  } // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa

  def nfaToDfa(nfa: Nfa[Int]): Dfa[Int] = {
    def getAlphabet(nfa: Nfa[Int]): Set[Char] = {
      nfa.transitions.keys.map(_._2).toSet.filterNot(_ == 'ε')
    }

    def getEpsilonClosure(nfa: Nfa[Int], state: Int): Set[Int] = {
      var epsilonClosure = Set(state)
      var newStates = Set(state)
      while (newStates.nonEmpty) {
        val nextStates = newStates.flatMap(s => nfa.next(s, 'ε'))
        newStates = nextStates.diff(epsilonClosure)
        epsilonClosure ++= newStates
      }
      epsilonClosure
    }

    def getEpsilonClosures(nfa: Nfa[Int]): Map[Int, Set[Int]] = {
      nfa.states.map(s => s -> getEpsilonClosure(nfa, s)).toMap
    }

    def subsetConstruction(startState: Int, nfaFinalStates: Set[Int], alphabet: Set[Char]): Dfa[Int] = {
      val epsilonClosures = getEpsilonClosures(nfa)
      var dfaStatesIntToSetInt = Map[Int, Set[Int]](-1 -> Set(-1))
      var dfaTransitionsSetToSet = Map[(Set[Int], Char), Set[Int]]()

      var dfaStateCounter = 0

      val dfaStartStateSet = epsilonClosures(startState)
      dfaStatesIntToSetInt += dfaStateCounter -> dfaStartStateSet

      def subsetConstructionRec(state: Set[Int]): Unit = {
        alphabet.foreach(c => {
          val nextStates = state.flatMap(nfa.next(_, c))
          if (nextStates.isEmpty) { //sink
            dfaTransitionsSetToSet += (state, c) -> Set(-1)
          } else {
            val epsilonClosure = nextStates.flatMap(epsilonClosures(_))
            if (!dfaStatesIntToSetInt.values.exists(_ == epsilonClosure)) {
              dfaStateCounter += 1
              dfaStatesIntToSetInt += dfaStateCounter -> epsilonClosure
              dfaTransitionsSetToSet += (state, c) -> epsilonClosure
              subsetConstructionRec(epsilonClosure)
            } else {
              dfaTransitionsSetToSet += (state, c) -> epsilonClosure
            }
          }
        })
      }

      subsetConstructionRec(dfaStartStateSet)

      val dfaStatesInt = dfaStatesIntToSetInt.keys.toSet
      val dfaTransitionsInt = dfaTransitionsSetToSet.map({ case ((s, c), v) => (dfaStatesIntToSetInt.find(_._2 == s).get._1, c) -> dfaStatesIntToSetInt.find(_._2 == v).get._1 })
      val dfaFinalStatesInt = dfaStatesIntToSetInt.filter({ case (_, v) => nfaFinalStates.intersect(v).nonEmpty }).keys.toSet
      //val dfaFinalTokenMap = nfa.tokenMap.filter({ case (_, v) => nfaFinalStates.intersect(v).nonEmpty }).map({ case (k, v) => k -> v.map(s => dfaStatesIntToSetInt.find(_._2 == epsilonClosures(s)).get._1) })
      //new dfaFinalTokenMap will contain all the tokens and thei corresponding final states after they have been converted
      //from the nfa states
      //val dfaFinalTokenMap = nfa.tokenMap.map({ case (k, v) => k -> v.map(s => dfaStatesIntToSetInt.find(_._2 == epsilonClosures(s)).getOrElse(s, Set())._1) })
      val dfaFinalTokenMap = nfa.tokenMap.map({case (k, v) => k -> dfaStatesIntToSetInt.filter({case (_, s) => s.intersect(v).nonEmpty}).keys.toSet})
      new Dfa(0, -1, dfaTransitionsInt, dfaFinalStatesInt, dfaStatesInt, dfaFinalTokenMap)
    }

    subsetConstruction(nfa.startState, nfa.finalStates, getAlphabet(nfa))
  }


  // You can add more methods to this object
}
