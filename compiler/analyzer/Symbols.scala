package punkt0
package analyzer

import Types._
import ast.Trees._

object Symbols {

  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol in " + this)
    }

    def hasSymbol: Boolean = _sym match {
      case Some(s) => true
      case None => false
    }

    def getClassSymbol: ClassSymbol = getSymbol match {
      case t: FunctionSymbol => t.getClassSymbol
      case t: MethodSymbol => t.classSymbol
      case t: ClassSymbol => t
      case _ => sys.error("Accessing undefined symbol in " + this)
    }
  }


  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
    def getClassSymbol: ClassSymbol = this match {
      case t: FunctionSymbol => t.parentSymbol.getClassSymbol
      case t: MethodSymbol => t.classSymbol
      case t: ClassSymbol => t
      case _ => sys.error("Accessing undefined symbol in " + this)
    }
    def getMethodSymbol: MethodSymbol = this match {
      case t: FunctionSymbol => t.parentSymbol.getMethodSymbol
      case t: MethodSymbol => t
      case t: ClassSymbol => sys.error("Accessing undefined symbol in " + this)
      case _ => sys.error("Accessing undefined symbol in " + this)
    }
    def lookupVar(n: String): Option[VarSymbol] = sys.error("Use of undefined function lookupVar")
  }


  // A var symbol is either a function symbol or a variable symbol
  sealed abstract class VarSymbol extends Symbol {
    var isFree: Boolean = false
  }


  // Generates IDs for names
  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  // Global scope remembers classes in the global scope
  class GlobalScope(val name: String) extends Symbol {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)
  }

  // A class symbol remembers its parent, methods, members and similar nice-to-know stuff
  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VarSymbol]()

    // Finds an optional method with the given name
    def lookupMethod(n: String): Option[MethodSymbol] = {
      methods.get(n) match {
        case Some(x) => Some(x)
        case None =>  parent match {
          case Some(y) => y.lookupMethod(n)
          case None => None
        }
      }
    }

    // Looks up a variable with the given name
    override def lookupVar(n: String): Option[VarSymbol] = {
      members.get(n) match {
        case Some(x) => Some(x)
        case None =>  parent match {
          case Some(y) => y.lookupVar(n)
          case None => None
        }
      }
    }
  }

  // Remembers parameters, members and parent 
  // What differs the params and argList are that they are good for different reasons
  // argList can be used when we want "the second parameter" and param when we want
  // "the symbol of the parameter named 'q'"
  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VarSymbol]()
    var argList: List[VarSymbol] = Nil
    var members = Map[String, VarSymbol]()
    var overridden: Option[MethodSymbol] = None

    // Looks up a variable with the given name
    override def lookupVar(n: String): Option[VarSymbol] = {
      members.get(n) match {
        case Some(x) => Some(x)
        case None => {
          params.get(n) match {
            case Some(x) => Some(x)
            case None => classSymbol.lookupVar(n)
          }
        }
      }
    }

    // String representation of method symbol
    override def toString(): String = {
      "MethodSymbol: { \n\tName:\t" + name + "\n\tClassSymbol:\t" + classSymbol + "\n\tParams:  \t" + params.toString + "\n\tArglist:\t" + params.toString + "\n}"
    }
  }

  // Remembers most of the stuff a variable and a method needs to remember
  class FunctionSymbol(val name: String, var className: String, val parentSymbol: Symbol) extends VarSymbol {
    var params = Map[String, VarSymbol]()
    var argList: List[VarSymbol] = Nil
    var freeVariables: Map[Identifier, String] = Map[Identifier, String]()

    // Looks up a variable with the given name    
    override def lookupVar(n: String): Option[VarSymbol] = params.get(n) match {
      case Some(x) => Some(x)
      case None => parentSymbol match {
        case r: MethodSymbol => r.lookupVar(n)
        case r: FunctionSymbol => r.lookupVar(n)
        case r => sys.error("Can't lookup on any other symbol")
      }
    }

    // String representation of function symbol
    override def toString(): String = {
      "FunctionSymbol: { \n\tName:\t" + name + "\n\tClassName:\t" + className + "\n\tParams:  \t" + params.toString + "\n\tArglist:\t" + params.toString + "\n\tFreeVariables:\t" + freeVariables + "\n}"
    }
  }

  class VariableSymbol(val name: String) extends VarSymbol
}