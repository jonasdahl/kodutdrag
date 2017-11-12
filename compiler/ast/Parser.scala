package punkt0
package ast

import Trees._
import lexer._
import scala.collection.mutable.ListBuffer

object Parser extends Phase[Iterator[Token], Program] {
  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    Reporter.errors = false
    import Reporter._

    // Store the current token, as read from the lexer
    var currentToken: Token = new Token(BAD)

    // Read one token, if there is
    // Skips bad tokens
    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    // ''Eats'' the expected token, or terminates with an error.
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    // Complains that what was found was not expected. 
    // The method accepts arbitrarily many arguments of type TokenKind
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    // Parses a program
    def parseGoal: Program = {
      val posToken = currentToken
      var classes = ListBuffer[ClassDecl]()
      while (isInFirst("ClassDeclaration")) {
        classes += parseClassDeclaration
      }
      val program = Program(parseMainDeclaration, classes.toList).setPos(posToken)
      eat(EOF)
      program
    }

    // Parses class declaration, calls parseClassBody to parse the body between brackets
    def parseClassDeclaration: ClassDecl = {
      val posToken = currentToken
      eat(CLASS)
      val name = parseIdentifier

      // If there is an extends statement, save that
      var parent = if(currentToken.kind == EXTENDS) {
        eat(EXTENDS); 
        Some(parseIdentifier)
      } else None

      val (varDecls, methods) = parseClassBody
      ClassDecl(name, parent, varDecls.toList, methods.toList).setPos(posToken)
    }

    // Parses a main declaration
    def parseMainDeclaration: MainDecl = {
      val posToken = currentToken
      eat(OBJECT)
      val name = parseIdentifier
      eat(EXTENDS)
      val parent = parseIdentifier
      val (varDecls, exprs) = parseMethodBody
      MainDecl(name, parent, varDecls.toList, exprs.toList).setPos(posToken)
    }

    // Parses a method declaration
    def parseMethodDeclaration: MethodDecl = {
      val posToken = currentToken

      // First check if there is override keyword
      var overrides = if (currentToken.kind == OVERRIDE) {
        eat(OVERRIDE)
        true
      } else false

      eat(DEF)
      val name = parseIdentifier
      eat(LPAREN)
      val formals = ListBuffer[Formal]()
      while (isInFirst("Formal")) {
        formals += parseFormal
        if (currentToken.kind == COMMA) {
          eat(COMMA)
        }
      }
      eat(RPAREN)
      eat(COLON)
      val retType = parseType
      eat(EQSIGN)
      val (varDecls, exprs) = parseMethodBody
      var retExpr = exprs.last
      MethodDecl(overrides, retType, name, formals.toList, varDecls.toList, exprs.toList.init, retExpr).setPos(posToken)
    }

    // Parses a variable declaration    
    def parseVarDeclaration: VarDecl = {
      val posToken = currentToken
      eat(VAR)
      val id = parseIdentifier
      eat(COLON)
      val typo = parseType
      eat(EQSIGN)
      val expression = parseExpression
      eat(SEMICOLON)
      VarDecl(typo, id, expression).setPos(posToken)
    }

    // Parses a type
    // A type can be for example:
    // * () => Boolean
    // * (Int, Int) => (Int, Int) => Boolean
    // * ((Int, Int) => (Int, Int)) => Boolean
    // * Int
    // * String
    // etc
    //
    // The final return type must not have parens
    def parseType: TypeTree = {
      val posToken = currentToken
      parseFunctionStepType.setPos(posToken)
    } 

    // Parses a type that could be a function type
    // Looks for an arrow after the first type, and if there is (sometimes, 
    // it has to be, for example when we have parsed a parameter list 
    // starting with parens) we assume it is a function type
    def parseFunctionStepType: TypeTree = {
      val posToken = currentToken
      val res = currentToken.kind match {
        case LPAREN => {
          eat(LPAREN)

          val argList = ListBuffer[TypeTree]()
          while (isInFirst("Type")) {
            argList += parseType
            if (currentToken.kind == COMMA) {
              eat(COMMA)
            }
          }
          eat(RPAREN)
          eat(ARROW)
          FunctionType(argList.toList, parseFunctionStepType)
        }
        case _ => {
          val p = parseSimpleType
          if (currentToken.kind == ARROW) {
            eat(ARROW)
            FunctionType(List(p), parseFunctionStepType)
          } else {
            p
          }
        }
      }
      res.setPos(posToken)
    }

    // Parses a simple type, ie types that are not functions
    def parseSimpleType: TypeTree = {
      val posToken = currentToken
      val res = currentToken.kind match {
        case BOOLEAN => {eat(BOOLEAN); BooleanType()}
        case STRING => {eat(STRING); StringType()} 
        case INT => {eat(INT); IntType()}    
        case UNIT => {eat(UNIT); UnitType()}
        case IDKIND => {parseIdentifier}
        case _ => expected(BOOLEAN, STRING, INT, UNIT, IDKIND)
      }
      res.setPos(posToken)
    }

    // Parses a new A() statement
    def parseNew: New = {
      val posToken = currentToken
      eat(NEW)
      val identifier = parseIdentifier
      eat(LPAREN)
      eat(RPAREN)
      New(identifier).setPos(posToken)
    }

    // Parses a string literal between "such quotes"
    def parseString: StringLit = {
      val posToken = currentToken
      val res = currentToken match {
        case t: STRLIT => {eat(STRLITKIND); StringLit(t.value).setPos(posToken)}
        case _ => expected(STRLITKIND)
      }
      res.setPos(posToken)
    }

    // Parses an integer literal
    def parseInt: IntLit = {
      val posToken = currentToken
      val res = currentToken match {
        case t: INTLIT => eat(INTLITKIND); IntLit(t.value).setPos(posToken)
        case _ => expected(INTLITKIND)
      }
      res.setPos(posToken)
    }

    // Parses an identifier 
    def parseIdentifier: Identifier = {
      val posToken = currentToken
      val res = currentToken match {
        case t: ID => {eat(IDKIND); Identifier(t.value).setPos(posToken)}
        case _ => expected(IDKIND)
      }
      res.setPos(currentToken)
    }

    // Parses an argument with type and name
    def parseFormal: Formal = {
      val posToken = currentToken
      val name = parseIdentifier
      eat(COLON)
      Formal(parseType, name).setPos(posToken)
    }

    def parseVarDeclarations: ListBuffer[VarDecl] = {
      val varDeclarations = ListBuffer[VarDecl]()
      while (isInFirst("VarDeclaration")) {
        varDeclarations += parseVarDeclaration
      }
      varDeclarations
    }

    // Parses the body of a method
    // Returns both a list of var declarations and a list of expressions
    def parseMethodBody: (ListBuffer[VarDecl], ListBuffer[ExprTree]) = {
      eat(LBRACE)
      
      var varDeclarations = parseVarDeclarations
      var expressions = ListBuffer[ExprTree]()
      expressions += parseExpression
      while(currentToken.kind == SEMICOLON) {
        eat(SEMICOLON)
        expressions += parseExpression
      }
      eat(RBRACE)
      (varDeclarations, expressions)
    }

    // Parses the body of a class
    // Returns both a list of var declarations and a list of expressions
    def parseClassBody: (ListBuffer[VarDecl], ListBuffer[MethodDecl]) = {
      eat(LBRACE)
      var varDeclarations = parseVarDeclarations
      var methods = ListBuffer[MethodDecl]()
      while(isInFirst("MethodDeclaration")) {
        methods += parseMethodDeclaration
      }
      eat(RBRACE)
      (varDeclarations, methods)
    }

    // The following parsing methods parse different things connected to 
    // expressions. The tails follow the grammar.
    def parseBeginWithID: ExprTree = {
      val posToken = currentToken
      val identifier = parseIdentifier
      val res = currentToken.kind match {
        case EQSIGN => {
          eat(EQSIGN)
          val expression = parseExpression
          Assign(identifier, expression).setPos(posToken)
        }
        case _ => identifier
      }
      res.setPos(posToken)
    }

    // Parses an expression
    // There are a lot of different expressions, for a complete list, see the grammar
    // The code follows the grammar very closely
    def parseExpression: ExprTree = {
      val posToken = currentToken
      val or = parseOr
      if (isInFirst("OrTail")) {
        parseOrTail(or)
      } else or.setPos(posToken)
    }

    def parseOr: ExprTree = {
      val posToken = currentToken
      val and = parseAnd
      if (isInFirst("AndTail")) {
        parseAndTail(and)
      } else and.setPos(posToken)
    }

    def parseOrTail(or: ExprTree): ExprTree = {

      if (currentToken.kind == OR) {
        eat(OR)
        parseOrTail(Or(or, parseOr).setPos(or))
      } else parseAndTail(or)
    }

    def parseAnd: ExprTree = {
      val comparator = parseComparator
      if (isInFirst("ComparatorTail")) {
        parseComparatorTail(comparator) 
      } else comparator
    }

    def parseAndTail(and: ExprTree): ExprTree = {
      if (currentToken.kind == AND) {
        eat(AND)
        parseAndTail(And(and, parseAnd).setPos(and))
      } else parseComparatorTail(and)
    }

    def parseComparator: ExprTree = {
      val posToken = currentToken
      val term = parseTerm
      if (isInFirst("TermTail")) {
        parseTermTail(term)
      } else term
    }

    def parseComparatorTail(comparator: ExprTree): ExprTree = {
      if(currentToken.kind == LESSTHAN) {
        eat(LESSTHAN)
        parseComparatorTail(LessThan(comparator, parseComparator).setPos(comparator))
      } else if (currentToken.kind == EQUALS) {
        eat(EQUALS)
        parseComparatorTail(Equals(comparator, parseComparator).setPos(comparator))
      } else parseTermTail(comparator)
    }

    def parseTerm: ExprTree = {
      val posToken = currentToken
      val factor = parseFactor
      if (first("FactorTail").contains(currentToken.kind)) {
        parseFactorTail(factor)
      } else factor.setPos(currentToken)
    }

    def parseTermTail(term: ExprTree): ExprTree = {
      if(currentToken.kind == PLUS) {
        eat(PLUS)
        parseTermTail(Plus(term, parseTerm).setPos(term))
      } else if(currentToken.kind == MINUS) {
        eat(MINUS)
        parseTermTail(Minus(term, parseTerm).setPos(term))
      } else parseFactorTail(term)
    }

    def parseFactorTail(factor: ExprTree): ExprTree = {
      val posToken = currentToken
      if (currentToken.kind == TIMES) {
        eat(TIMES)
        parseFactorTail(Times(factor, parseFactor).setPos(factor))
      } else if (currentToken.kind == DIV) {
        eat(DIV)
        parseFactorTail(Div(factor, parseFactor).setPos(factor))
      } else factor
    }

    def parseFactor: ExprTree = {
      val posToken = currentToken
      val id = parseId
      if (first("DottyTail").contains(currentToken.kind)) {
        parseDottyTail(id)
      } else if (currentToken.kind == LPAREN) {
        parseFunctionCallTail(id)
      } else id
    }

    def parseFunctionCallTail(base: ExprTree): ExprTree = {
      val posToken = currentToken
      eat(LPAREN)
      var args = ListBuffer[ExprTree]()
      if (isInFirst("Expression")) {
        args += parseExpression
        while (currentToken.kind == COMMA) {
          eat(COMMA)
          args += parseExpression
        }
      }
      eat(RPAREN)
      val fc = base match {
        case t: Identifier => FunctionCall(t, args.toList)
        case t: FunctionCall => FunctionCall(t, args.toList)
        case _ => expected(IDKIND)
      }
      if (currentToken.kind == LPAREN) {
        parseFunctionCallTail(fc.setPos(posToken)).setPos(posToken)
      } else fc.setPos(posToken)
    }

    def parseDottyTail(dotty: ExprTree): ExprTree = {
      val posToken = currentToken
      eat(DOT)
      val name = parseIdentifier
      eat(LPAREN)
      var args = ListBuffer[ExprTree]()
      if(first("Expression").contains(currentToken.kind)) {
        args += parseExpression
        while(currentToken.kind == COMMA) {
          eat(COMMA)
          args += parseExpression
        }
      }
      eat(RPAREN)
      val mc: MethodCall = MethodCall(dotty, name, args.toList).setPos(dotty)
      if(first("DottyTail").contains(currentToken.kind)) {
        return parseDottyTail(mc)
      }
      mc
    }

    def parseId: ExprTree = {
      val posToken = currentToken
      val ret = currentToken.kind match {
        case INTLITKIND => parseInt
        case STRLITKIND => parseString
        case TRUE => {eat(TRUE); True().setPos(posToken)}
        case FALSE => {eat(FALSE); False().setPos(posToken)}
        case NULL => {eat(NULL); Null().setPos(posToken)}
        case THIS => {eat(THIS); This().setPos(posToken)}
        case NEW => parseNew
        case IDKIND => parseBeginWithID
        case BANG => parseBang
        case LPAREN => parseParen
        case LBRACE => parseBrace
        case IF => parseIf
        case WHILE => parseWhile
        case PRINTLN => parsePrintln
        case _ => expected(INTLITKIND, STRLITKIND, TRUE, FALSE, NULL, THIS, NEW, IDKIND, BANG, LPAREN, LBRACE, IF, WHILE, PRINTLN)
      } 
      ret.setPos(posToken)
    }

    def parseBang: Not = {
      val posToken = currentToken
      eat(BANG)
      Not(parseFactor).setPos(posToken)
    }

    def parseFunctionDecl(i: Option[Identifier] = None): ExprTree = {
      var args = ListBuffer[Formal]()
      i match {
        case Some(x) => {
          eat(COLON)
          val tpe = parseType
          args += Formal(tpe, x)
        }
        case None => 
      }
      while (currentToken.kind == COMMA) {
        eat(COMMA)
        val argId = parseIdentifier
        eat(COLON)
        val argType = parseType
        args += Formal(argType, argId)
      }
      eat(RPAREN)
      eat(ARROW)
      val block = parseExpression
      FunctionDecl(args.toList, block)
    }

    def parseParen: ExprTree = {
      val posToken = currentToken
      eat(LPAREN)
      val res = if (currentToken.kind == RPAREN) {
        parseFunctionDecl()
      } else {
        parseOr match {
          case t: Identifier => {
            val x = if (currentToken.kind == COLON) {
              parseFunctionDecl(Some(t))
            } else {
              val z = if (first("OrTail").contains(currentToken.kind)) {
                parseOrTail(t)
              } else {
                t
              }
              eat(RPAREN)
              z
            }
            x
          }
          case x => {
            val p = parseOrTail(x)
            eat(RPAREN)
            p
          }
        }
      }

      res.setPos(posToken)
    }

    def parseBrace: Block = {
      val posToken = currentToken
      var exprs = ListBuffer[ExprTree]()
      eat(LBRACE)
      if(first("Expression").contains(currentToken.kind)) {
        exprs += parseExpression
        while(currentToken.kind == SEMICOLON) {
          eat(SEMICOLON)
          exprs += parseExpression
        }
      }
      eat(RBRACE)
      Block(exprs.toList).setPos(posToken).setPos(posToken)
    }

    def parseIf: If = {
      val posToken = currentToken
      eat(IF)
      eat(LPAREN)
      val ifExpr = parseExpression
      eat(RPAREN)
      val thenExpr = parseExpression
      var isElse: Option[ExprTree] = None
      if(currentToken.kind == ELSE) {
        eat(ELSE)
        isElse = Some(parseExpression)
      }
      If(ifExpr, thenExpr, isElse).setPos(posToken)
    }

    def parseWhile: While = {
      val posToken = currentToken
      eat(WHILE)
      eat(LPAREN)
      val expr = parseExpression
      eat(RPAREN)
      While(expr, parseExpression).setPos(posToken)
    }

    def parsePrintln: Println = {
      val posToken = currentToken
      eat(PRINTLN)
      eat(LPAREN)
      val expr = parseExpression
      eat(RPAREN)
      Println(expr).setPos(posToken)
    }

    def first(nonTerminal: String): List[TokenKind] = {
      nonTerminal match {
        case "Type" => List(BOOLEAN, INT, STRING, UNIT, IDKIND, LPAREN)
        case "VarDeclaration" => List(VAR)
        case "Formal" => List(IDKIND)
        case "MethodDeclaration" => List(OVERRIDE, DEF)
        case "ClassDeclaration" => List(CLASS)
        case "Expression" => first("Dotty")
        case "DottyTail" => List(DOT)
        case "Dotty" => first("Or")
        case "Or" => first("And")
        case "OrTail" => List(OR)
        case "And" => first("Comparator")
        case "AndTail" => List(AND)
        case "ComparatorTail" => List(LESSTHAN, EQUALS)
        case "Comparator" => first("Term")
        case "TermTail" => List(PLUS, MINUS)
        case "Term" => first("Factor")
        case "FactorTail" => List(TIMES, DIV)
        case "Factor" => List(STRLITKIND, INTLITKIND, TRUE, FALSE, THIS, NULL, NEW, BANG, LPAREN, LBRACE, IF, WHILE, PRINTLN, IDKIND)
        case "Identifier" => List(IDKIND)
        case _ => throw new Exception("HAAAAALLLLLÅÅÅÅÅ")
      }
    }

    def isInFirst(tokenClass: String): Boolean = first(tokenClass) contains currentToken.kind

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
