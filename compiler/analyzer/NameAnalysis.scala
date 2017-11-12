package punkt0
package analyzer

import ast.Trees._
import Symbols._
import Types._
import Reporter._

/**
 * Name analysis assigns symbols to every name.
 */
object NameAnalysis extends Phase[Program, Program] {

  // Scope for remembering class references
  var global = new GlobalScope("");

  var functionId = 0;
  def getFunctionId: Int = {
    functionId += 1
    functionId
  }

  // First runs the symbolizer that makes all names Symbols
  // Then links all the symbols so that they are the correct symbols
  // the linking step also controls all constraints
  def run(prog: Program)(ctx: Context): Program = {
    // Reset state
    errors = false
    global = new GlobalScope("")

    linkProgram(prog)
    prog.setSymbol(global)
    terminateIfErrors
    prog
  }

  // Links program, main classes and classes recursively
  def linkProgram(prog: Program): Program = {
    prog.classes.foreach(linkClass)
    prog.classes.foreach(linkClassContent)
    linkMain(prog.main)
  	prog
  }

  // Links main decl symbol. We do not need to link obj since we see it as a base.
  def linkMain(m: MainDecl) {
    val mainSymbol = new ClassSymbol("Main")

    if (m.obj.value != "Main") error("Wrong name of Main object", m.obj)
    
    m.setSymbol(mainSymbol)
    m.obj.setSymbol(mainSymbol)
    global.mainClass = mainSymbol

    if (m.parent.value != "App") {
      error("Main object must extend App", m.parent)
    }
    val appSym = new ClassSymbol("App");
    appSym.setType(new TAnyRef(appSym));
    m.parent.setSymbol(appSym);
    m.vars.foreach(x => linkMember(x, mainSymbol))
    // Create fake method symbol to send to vars and exprs
    m.exprs.foreach(x => linkExpr(x, Some(new MethodSymbol(mainSymbol.name, mainSymbol))))
  }

  // Links a class
  def linkClass(c: ClassDecl) {
    val className = c.id.value
    val sym = new ClassSymbol(className);

    global.lookupClass(className) match {
      case Some(x) => error("class " + className + " already defined at " + x.posString, c)
      case None => {
        c.setSymbol(sym)
        c.id.setSymbol(sym)
        global.classes = global.classes + (className -> sym)
        sym.setType(new TAnyRef(sym))
      }
    }
  }

  // Link the content of a class
  def linkClassContent(c: ClassDecl) {
    // Link parent
    c.parent match {
      case Some(id) => global.lookupClass(id.value) match {
        case Some(x) => {
          id.setSymbol(x)
          c.getSymbol.parent = Some(x)
          if (inherits(c.getSymbol, c.getSymbol)) error("cycle in inheritance graph", c)
        }
        case None => error("inheriting " + id.value + " not possible, class " + id.value + " not defined", c)
      }
      case None => // Ok without inheritance
    }

    // Link variables
    c.vars.map(x => linkMember(x, c.getSymbol))
    c.methods.map(x => linkMethod(x, c.getSymbol))
  }

  // Links a class field
  def linkMember(v: VarDecl, classSymbol: ClassSymbol) {
    val varName = v.id.value

    // Create the symbol depending on type of the declaration
    val sym = v.tpe match {
      case t: FunctionType => new FunctionSymbol(varName, classSymbol.name + "$AnonFunc$" + getFunctionId, new MethodSymbol("", classSymbol))
      case _ => new VariableSymbol(varName)
    }

    classSymbol.lookupVar(varName).map(x => error("variable " + varName + " already defined at " + x.posString, v))

    v.setSymbol(sym)
    v.id.setSymbol(sym)
    classSymbol.members = classSymbol.members + (varName -> sym)

    linkType(v.tpe)
    v.expr match {
      case t: FunctionDecl => linkExpr(t, Some(new MethodSymbol("", classSymbol)))
      case t => linkConstant(t)
    }
  }

  // Links a method
  def linkMethod(m: MethodDecl, classSymbol: ClassSymbol) {
    val methName = m.id.value
    val result = classSymbol.methods.get(methName)
    var sym = new MethodSymbol(methName, classSymbol)
    m.setSymbol(sym)
    m.id.setSymbol(sym)
    result match {
      case Some(x) => {
        error("method " + methName + " already defined at " + x.posString, m)
      }
      case None => {
        classSymbol.lookupMethod(methName) match {
          case Some(y) => if(!m.overrides) error("method " + methName + " has the same name as a parent method but does not explicitly override", m)
          case None => if(m.overrides) error("method " + methName + " overriding undefined method ", m)
        }
        classSymbol.methods = classSymbol.methods + (methName -> sym)
      }
    }
    m.getSymbol.argList = List[VariableSymbol]()
    m.args.map(x => linkArg(x, sym))
    m.vars.map(x => linkVar(x, sym))
    m.exprs.map(x => linkExpr(x, Some(sym)))
    linkType(m.retType)
    linkExpr(m.retExpr, Some(m.getSymbol))
    if (m.overrides) checkOverridingMethod(m, classSymbol)
  }

  // Links method argument. 
  def linkArg(a: Formal, methodSymbol: MethodSymbol) {
    val argSym = a.tpe match {
      case t: FunctionType => new FunctionSymbol(a.id.value, methodSymbol.getClassSymbol.name + "$AnonFunc$" + getFunctionId, methodSymbol)
      case _ => new VariableSymbol(a.id.value)
    }

    // Set symbols for both whole formal and identifier
    a.setSymbol(argSym)
    a.id.setSymbol(argSym)

    methodSymbol.params.get(a.id.value) match {
      // a is an argument of a method, check that the name is not taken
      case Some(x) => error("argument " + a.id.value + " already defined at " + x.posString, a)
      case None => 
    }
    methodSymbol.params = methodSymbol.params + (a.id.value -> argSym)
    methodSymbol.argList = methodSymbol.argList :+ argSym
    linkType(a.tpe)
    a.id.getSymbol.setType(a.tpe.getType)
  }

  // Links lambda argument
  def linkLambdaArg(a: Formal, functionSymbol: FunctionSymbol) {
    val argSym = a.tpe match {
      case t: FunctionType => new FunctionSymbol(a.id.value, functionSymbol.getClassSymbol.name + "$AnonFunc$" + getFunctionId, functionSymbol)
      case _ => new VariableSymbol(a.id.value)
    }

    // Set symbols for both whole formal and identifier
    a.setSymbol(argSym)
    a.id.setSymbol(argSym)

    functionSymbol.params.get(a.id.value) match {
      // a is an argument of a method, check that the name is not taken
      case Some(x) => error("argument " + a.id.value + " already defined at " + x.posString, a)
      case None => 
    }
    functionSymbol.params = functionSymbol.params + (a.id.value -> argSym)
    functionSymbol.argList = functionSymbol.argList :+ argSym
    linkType(a.tpe)
    a.id.getSymbol.setType(a.tpe.getType)
  }

  // Links variable declaration
  def linkVar(v: VarDecl, methodSymbol: MethodSymbol) {
    val varName = v.id.value
    val sym = v.expr match {
      case t: FunctionDecl => {
        linkExpr(t, Some(methodSymbol))
        val xSym = t.getSymbol
        t.getSymbol
      }
      case t => new VariableSymbol(varName)
    }

    methodSymbol.params.get(varName) match {
      case Some(x) => error("variable " + varName + " already defined as a parameter at " + x.posString, v)
      case None => 
    }
    methodSymbol.members.get(varName) match {
      case Some(x) => error("variable " + varName + " already defined at " + x.posString, v)
      case None =>
    }

    v.setSymbol(sym)
    v.id.setSymbol(sym)
    methodSymbol.members = methodSymbol.members + (varName -> sym)
    linkType(v.tpe)
    v.id.getSymbol.setType(v.tpe.getType) // Keep?

    v.expr match {
      case t: FunctionDecl => linkExpr(t, Some(methodSymbol))
      case t => linkConstant(t)
    }
  }

  // Links a type
  def linkType(t: TypeTree) {
    t match {
      case t: Identifier => {
        global.lookupClass(t.value) match {
          case Some(x) => {t.setSymbol(x)}
          case None => error("class " + t.value + " not defined", t)
        }
      }
      case t: IntType => 
      case t: StringType =>
      case t: BooleanType =>
      case t: UnitType =>
      case t: FunctionType => t.args.foreach(linkType); linkType(t.ret)
      case _ => error("not a valid type", t)
    }
  }

  // Links constant
  def linkConstant(e: ExprTree) {
    e match {
      case t: IntLit =>
      case t: StringLit =>
      case t: True =>
      case t: False =>
      case t: New => linkType(t.tpe)
      case t: Null => 
      case t => error("init statement must be constant, null or a 'new expression', found " + t, e)
    }
  }

  // Here most intersting stuff happens
  // Links expressions and calls recursively on branching expressions
  def linkExpr(e: ExprTree, sym: Option[MethodSymbol], funcSym: Option[FunctionSymbol] = None) {
    def le(e: ExprTree) {
      e match {
        case          Not(l) => le(l)
        case      Println(x) => le(x)
        case      While(c,b) => le(c); le(b)
        case        And(l,r) => le(l); le(r)
        case         Or(l,r) => le(l); le(r)
        case       Plus(l,r) => le(l); le(r)
        case      Minus(l,r) => le(l); le(r)
        case      Times(l,r) => le(l); le(r)
        case        Div(l,r) => le(l); le(r)
        case   LessThan(l,r) => le(l); le(r)
        case     Equals(l,r) => le(l); le(r)
        case        New(tpe) => linkType(tpe) 
        case       t: IntLit => t.setType(TInt)
        case    t: StringLit => t.setType(TString)  
        case         t: True => t.setType(TBoolean)  
        case        t: False => t.setType(TBoolean) 
        case        Block(e) => e.foreach(x => le(x))
        case       If(i,t,e) => le(i); le(t); e.map(x => le(x))
        case         t: This => t.setSymbol(sym.getOrElse(funcSym.get).getClassSymbol).setType(sym.getOrElse(funcSym.get).getClassSymbol.getType)
        case t: FunctionDecl => { 
          val someSym = sym.getOrElse(funcSym.get).getMethodSymbol
          val resSym = new FunctionSymbol("", someSym.getClassSymbol.name + "$AnonFunc$" + getFunctionId, someSym)
          t.args.foreach(x => linkLambdaArg(x, resSym)) // Add the args
          linkExpr(t.expr, None, Some(resSym))
          t.args.foreach(x => {
            resSym.params += (x.id.value -> x.getSymbol)
            resSym.argList = resSym.argList :+ x.getSymbol
          })
          t.setSymbol(resSym)
        }
        case t: FunctionCall => { 
          le(t.meth)
          t.args.foreach(le)
          val s = t.meth match {
            case i: Identifier => i.getSymbol match {
              case fs: FunctionSymbol => fs
              case _ => error(i.value + " is not a function", i); new FunctionSymbol("", "", sym.getOrElse(funcSym.get).getMethodSymbol)
            }
            case fc: FunctionCall => fc.getSymbol
            case _ => sys.error("Unexpected end")
          }
          t.setSymbol(s)
        } 
        case MethodCall(o,m,a) => { 
          le(o)
          m.setSymbol(new MethodSymbol(m.value, new ClassSymbol("Temp")))
          a.foreach(le) 
        } 
        case v: Identifier => { 
          sym match {
            case Some(x) => x.lookupVar(v.value) match {
              case Some(x) => v.setSymbol(x)
              case None => error("variable " + v.value + " not defined", v)
            }
            case None => funcSym.get.lookupVar(v.value) match {
              case Some(x) => v.setSymbol(x)
              case None => error("variable " + v.value + " not defined", v)
            }
          }
        }
        case   Assign(i,e) => { 
          sym.getOrElse(funcSym.get).getMethodSymbol.params.get(i.value) match {
            case Some(x) => error("can't reassign parameter " + i, i)
            case None =>
          }
          le(i)
          le(e)
        }
        case Null() =>
      }
    }
    le(e)
  }

  // Checks if method decl overrides and if so, if it is correctly overridden
  def checkOverridingMethod(m: MethodDecl, classSymbol: ClassSymbol) = {
    classSymbol.parent match {
      case Some(parent) => {
        parent.lookupMethod(m.id.value) match {
          case Some(methodParent) => if (m.args.length != methodParent.params.keySet.size) {
            error("wrong number of parameters in overriding method " + m.id.value + " in position " + m.posString)
          }
          case None => throw new Exception("Internal error: we're in deep trouble")
        }
      }
      case None => throw new Exception("Internal error: couldn't find overriding method")
    }
  }

  // Checks if in any way c1 inherits c2
  def inherits(c1: ClassSymbol, c2: ClassSymbol): Boolean = {
    c1.parent match {
      case Some(x) => {
        if(x == c2) true
        else inherits(x, c2)
      } 
      case None => false
    }
  }
}