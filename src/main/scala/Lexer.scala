case class Lexer (spec: String) {

  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */
  def lex(word: String): Either[String, List[(String, String)]] = {
    val specLines = spec.split(';').map(_.trim).toList.init
    var nfas = Map[String, Nfa[Int]]()

    specLines.foreach(l => {
      if (l.split(':').length != 2) {
        return Left("Invalid specification")
      }

      val token = l.split(':')(0)
      val regex = l.split(':')(1).trim

      if (token.isEmpty || regex.isEmpty) {
        return Left("Invalid specification")
      }

      val prenex = Regex.toPrenex(regex)
      //println(s"Token: $token, Regex: $regex, Prenex: $prenex")
      val nfa = Nfa.fromPrenex(prenex, token)
      //nfa.print()
      nfas += (token -> nfa)
    })

//    if(specTokens.intersect(List("SPACE", "NEWLINE")).isEmpty) {
//      word = word.replaceAll(" ", "").replaceAll("\n", "")
//    }

    //println("BIG NFA\n")
    var bigNfa = new Nfa[Int](0, Map(), Set(), Set(0), Map())
    nfas.foreach( n => {
      //bigNfa.print()
      val states = n._2.getStates
      val finalStates = n._2.getFinalStates
      val transitions = n._2.getTransitions
      val tokenMap = n._2.getTokenMap

      val newStart = bigNfa.getStates.size
      val newStates = states.map(s => s + newStart)
      val newFinalStates = finalStates.map(s => s + newStart)
      var newTransitions = transitions.map({case ((s, c), a) => (s + newStart, c) -> a.map(s => s + newStart)})
      newTransitions += ((0, 'ε') -> (bigNfa.getTransitions.getOrElse((0, 'ε'), Set()) ++ Set(newStart)))
      val newTokenMap = tokenMap.map({case (s, a) => s -> a.map(s => s + newStart)})

      bigNfa = new Nfa[Int](0, bigNfa.getTransitions ++ newTransitions, bigNfa.getFinalStates ++ newFinalStates, bigNfa.getStates ++ newStates, bigNfa.getTokenMap ++ newTokenMap)
    })

    //bigNfa.print()
    //println("Big nfa nr of states" + bigNfa.getStates.size)

    val dfa = Dfa.nfaToDfa(bigNfa)

    //println("BIG DFA\n")
    //dfa.print()

    def getLexems(word: String, dfa: Dfa[Int]): Either[String, List[(String, String)]] = {
      var lexems = List[(String, String)]()
      var state = dfa.getStartState
      var lexem = ""
      var token = ""
      var tokensFound = List[String]()
      var lexemsFound = List[String]()

      var lineCount = 0
      var charCount = 0
      var last_i_accepted = 0
      var i = 0
      //println("Word: " + word + " length: " + word.length)

      while (last_i_accepted < word.length) {
        i = last_i_accepted
        charCount = last_i_accepted

        while (i < word.length) {
          val c = word(i)
          state = dfa.next(state, c)
          if (state == -1) {
            if (tokensFound.isEmpty) {
              return Left(s"No viable alternative at character $charCount, line $lineCount")
            } else {
              token = tokensFound.last
              lexem = lexemsFound.last
              lexems = lexems :+ (lexem, token)
              tokensFound = List[String]()
              lexemsFound = List[String]()
              lexem = ""
              token = ""
              state = dfa.getStartState
              i = last_i_accepted + 1
              charCount = last_i_accepted + 1
            }
          } else {
            if (dfa.isFinal(state)) {
              last_i_accepted = i
              tokensFound = tokensFound :+ dfa.getTokenMap.find(_._2.contains(state)).get._1
              lexemsFound = lexemsFound :+ lexem + c
              //println(tokensFound)
            }
            lexem += c
            i += 1
            charCount += 1
            if (c == '\n') {
              lineCount += 1
              charCount = 0
            }

          }
        }

        if (tokensFound.nonEmpty) {
          token = tokensFound.last
          lexem = lexemsFound.last
          lexems = lexems :+ (lexem, token)

        } else {
          return Left(s"No viable alternative at character EOF, line $lineCount")
        }

        tokensFound = List[String]()
        lexemsFound = List[String]()
        lexem = ""
        token = ""
        state = dfa.getStartState
        last_i_accepted += 1
      }

      if (lexems.isEmpty) {
        return Left(s"No viable alternative at character EOF, line $lineCount")
      }

      //println(lexems)
      Right(lexems)
    }

    val output = getLexems(word, dfa)
    //println(output)
    output

    //Left("Not implemented yet")
  }
}
