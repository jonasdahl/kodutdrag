package punkt0
package lexer

import java.io.File

object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  // Returns an iterator of tokens from the input file
  def run(f: File)(ctx: Context): Iterator[Token] = {

    val source = scala.io.Source.fromFile(f)

    new Iterator[Token] {
      var current: Option[Token] = None
      var done: Boolean = false
      var char: Char = 0

      val keyWords = Map(
        "object" -> OBJECT, "class"   -> CLASS,   "def"     -> DEF,    "override" -> OVERRIDE, 
        "var"    -> VAR,    "Unit"    -> UNIT,    "String"  -> STRING, "extends"  -> EXTENDS, 
        "Int"    -> INT,    "Boolean" -> BOOLEAN, "while"   -> WHILE,  "if"       -> IF, 
        "else"   -> ELSE,   "true"    -> TRUE,    "false"   -> FALSE,  "this"     -> THIS, 
        "null"   -> NULL,   "new"     -> NEW,     "println" -> PRINTLN
      )

      val letter = "[a-zA-Z]".r
      val positiveDigit = "[1-9]".r
      val digit = "[0-9]".r
      val whiteSpace = "\\s+".r
      val cite = """(")""".r

      def hasNext: Boolean = current match {
        case Some(token) => if(token.toString == "EOF") false else true
        case _ => true
      }

      def next: Token = {
        nextToken
      }

      def nextToken: Token = {
        var needsConsuming = true
        var pos = source.pos
        var errMsg = ""

        if (done) {
          terminateIfErrors
          current = Some(new Token(EOF)) 
          val token = current.get
          token.setPos(f, pos)
          return token
        } 

        char match {
          case 0 => 
          case whiteSpace(_*) => current = None
          case ':' => current = Some(new Token(COLON))
          case ';' => current = Some(new Token(SEMICOLON))
          case '.' => current = Some(new Token(DOT))
          case ',' => current = Some(new Token(COMMA))
          case '=' => {
            if(source.hasNext) {
              char = source.next 
              if(char == '=') {
                current = Some(new Token(EQUALS))
              }
              else if(char == '>') {
                current = Some(new Token(ARROW))
              }
              else {current = Some(new Token(EQSIGN)); needsConsuming = false}
            }
            else current = Some(new Token(EQSIGN))
          }
          case '!' => current = Some(new Token(BANG))
          case '(' => current = Some(new Token(LPAREN))
          case ')' => current = Some(new Token(RPAREN))
          case '{' => current = Some(new Token(LBRACE))
          case '}' => current = Some(new Token(RBRACE))
          case '&' => {
            if(source.hasNext) {
              char = source.next 
              if(char == '&') {
                current = Some(new Token(AND))
              }
              else {
                current = Some(new Token(BAD))
                errMsg = "single &"
                needsConsuming = false
              }
            }
            else {
              current = Some(new Token(BAD))
              errMsg = "single &"
            }
          }
          case '|' => {
            if(source.hasNext) {
              char = source.next 
              if(char == '|') {
                current = Some(new Token(OR))
              }
              else {
                current = Some(new Token(BAD)) 
                errMsg = "single |"
                needsConsuming = false
              }
            }
            else {current = Some(new Token(BAD)); errMsg = "single |"}
          }
          case '<' => current = Some(new Token(LESSTHAN))
          case '+' => current = Some(new Token(PLUS))
          case '-' => current = Some(new Token(MINUS))
          case '*' => current = Some(new Token(TIMES))
          case '/' => {
            if(source.hasNext) {
              char = source.next
              if(char == '/') {
                while(source.hasNext && char != '\n' && char != '\r') {
                  char = source.next
                }
                if(!source.hasNext) current = Some(new Token(EOF));
                else current = None
              }
              else if(char == '*') {
                var finishedComment = false
                while(source.hasNext && !finishedComment) {
                  char = source.next
                  if(source.hasNext && char == '/') {
                    char = source.next
                    if(char == '*') {
                      return new Token(BAD).setPos(f, pos)
                      errMsg = "nested comment"
                    }
                  }
                  if(source.hasNext && char == '*') {
                    char = source.next
                    if(char == '/') {finishedComment = true; current = None}
                  }
                }
                if(!source.hasNext && !finishedComment) {
                  current = Some(new Token(BAD))
                  errMsg = "unclosed comment"
                  needsConsuming = false
                }
              }
              else {current = Some(new Token(DIV)); needsConsuming = false}
            }
            else current = Some(new Token(DIV))
          }
          case letter(_*) => {
            val b = new StringBuilder
            b.append(char)
            while (source.hasNext && isIDChar({char = source.next; char})) {
              b.append(char)
            }
            val string = b.toString
            if(keyWords.contains(string)) current = Some(new Token(keyWords.get(string).get))
            else current = Some(new ID(string))
            if(!isIDChar(char)) needsConsuming = false
          }
          case cite(_*) => {
            if(source.hasNext) {
              char = source.next
              val b = new StringBuilder
              if (char != '\"') {
                b.append(char)
                while (source.hasNext && isStringLitChar({char = source.next; char})) {
                  b.append(char)
                }
              }
              val string = b.toString
              if (char == '\"') {
                current = Some(new STRLIT(string))
              }
              else {current = Some(new Token(BAD)); errMsg = "unclosed string"}
            }
            else {current = Some(new Token(BAD)); errMsg = "unclosed string"}
          }
          case positiveDigit(_*) => {
            var k = Integer.parseInt(char.toString)
            while (source.hasNext  && isDigit({char = source.next; char})) {
              k = 10*k + Integer.parseInt(char.toString)
            }
            current = Some(new INTLIT(k))
            if(!isDigit(char)) needsConsuming = false
          }
          case digit(_*) => {
            // Currenttoken was 0
            current = Some(new INTLIT(0))
          }
          case _ => {
            current = Some(new Token(BAD))
            errMsg = "invalid symbol"
          }
        }

        if(source.hasNext) {
          if(needsConsuming) char = source.next
        } else if(needsConsuming) done = true

        current match {
          case Some(token) => {
            token.setPos(f, pos)
            if(errMsg != "") {errors = true; error(errMsg, token)}
            token
          }
          case None => nextToken
        }
      }

      def isLetter(ch: Char): Boolean = {
        val letter = "[a-zA-Z]".r
        ch match {
          case letter(_*) => true
          case _ => false
        }
      }

      def isDigit(ch: Char): Boolean = {
        val digit = "[0-9]".r
        ch match {
          case digit(_*) => true
          case _ => false
        }
      }

      def isIDChar(ch: Char): Boolean = {
        val letter = "[a-zA-Z0-9_]".r
        ch match {
          case letter(_*) => true
          case _ => false
        }
      }

      def isStringLitChar(ch: Char): Boolean = {
        val invalid = "[^\n\"\r]".r
        ch match {
          case invalid(_*) => true
          case _ => false
        }
      }
    }
  }
}
