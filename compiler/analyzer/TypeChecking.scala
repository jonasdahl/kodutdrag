package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {
  import Reporter._

  var varId = 0

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(prog: Program)(ctx: Context): Program = {
    errors = false
    prog.classes.foreach(tcClassSignatures)
    prog.classes.foreach(tcClassContent)
    tcMain(prog.main)
    terminateIfErrors
    prog
  }

  // Type checks main
  def tcMain(m: MainDecl) {
    m.vars.foreach(tcVar)
    m.exprs.foreach(x => tcExpr(x))
  }

  // Type checks class
  def tcClassSignatures(c: ClassDecl) {
    c.methods.foreach(tcMethod)
  }

  // Type checks class
  def tcClassContent(c: ClassDecl) {
    c.vars.foreach(tcVar)
    c.methods.foreach(tcMethodContents)
  }

  def freshVarId: Int = {
    varId = varId + 1;
    varId
  }

  // Type checks var assignment
  def tcVar(v: VarDecl) {
    tcTypeTree(v.tpe)
    tcExpr(v.expr, v.tpe.getType)
    v.id.getSymbol.setType(v.tpe.getType)
    v.id.setType(v.tpe.getType)
    v.getSymbol.setType(v.tpe.getType)
  }

  // Type checks a type tree and sets a type for it
  def tcTypeTree(t: TypeTree): Type = {
    val x = t match {
      case _: BooleanType => TBoolean
      case _: IntType     => TInt
      case _: StringType  => TString
      case _: UnitType    => TUnit
      case c: FunctionType=> TFunction(c.args.map(tcTypeTree), tcTypeTree(c.ret))
      case q: Identifier  => q.getType
      case _ => TError
    }
    t.setType(x)
    x
  }

  // Type checks method signatures
  def tcMethod(m: MethodDecl) {
    m.args.foreach(tcFormal)

    // Type assign return type
    tcTypeTree(m.retType)
    m.getSymbol.setType(m.retType.getType)

    // Check type of params if overriding
    val methName = m.id.value
    m.getSymbol.classSymbol.parent match {
      case Some(parent) => parent.lookupMethod(methName) match {
        case Some(methodParent) => {
          (m.args, methodParent.argList).zipped.foreach((x, y) => {
            if (x.tpe.getType != y.getType)
              error("ouch! The types of parameters for overriding methods don't match. But don't panic, the error is for parameter " + x.id.value + "! \n Expected type " + y.getType + " but found " + x.tpe.getType + ". The method overridden is in class " + parent.name + ".", x)
          })
          if (m.retType.getType != methodParent.getType) {
            error("oh no! The return type of method " + methName + " in class " + m.getSymbol.classSymbol.name + " does not match the overridden method in class " + parent.name + ". We don't know what to do then so we will exit soon!", m.retType)
          }
        }
        case None => //Already checked, so should never happen maybe throw actual exception? internal error
      }
      case None =>
    }
  }

  // Type checks method contents
  def tcMethodContents(m: MethodDecl) {
    m.vars.foreach(tcVar)
    m.exprs.foreach(e => tcExpr(e))

    // Type check return type
    tcExpr(m.retExpr, m.retType.getType)
  }

  // Type check formal, ie argument in argument list
  def tcFormal(f: Formal) {
    tcTypeTree(f.tpe)
    f.id.getSymbol.setType(f.tpe.getType)
  }

  // Type check expressions
  def tcExpr(expr: ExprTree, expected: Type*): Type = {
    val tpe: Type = expr match {
      case t: FunctionDecl => {
        t.args.foreach(x => tcFormal(x))
        val tpe = TFunction(t.args.map(_.tpe.getType), tcExpr(t.expr))
        t.getSymbol.freeVariables = findFreeVariables(t, t.getSymbol).distinct.map(x => (x, x.value + "$" + freshVarId)).toMap
        tpe
      }
      case t: FunctionCall => {
        tcExpr(t.meth)
        val s = t.getSymbol
        val tf = t.meth.getType match {
          case t: TFunction => t 
          case _ => error("Unexpected function call on non-function", t); return TError
        }
        // Match every argument type and check that the parameter list has the same types
        if (tf.args.size != t.args.size) {
          error("Wrong number of arguments, expected " + tf.args.size + ", got " + t.args.size + " in " + t, if (t.args.size > 0) t.args.head else t)
        }
        (t.args, tf.args).zipped.foreach((a, b) => {
          tcExpr(a, b)
        })
        tf.ret
      }
      case t: Plus       => {
        var t1 = tcExpr(t.lhs, TInt, TString)
        var t2 = tcExpr(t.rhs, TInt, TString)
        if (t1 == t2) {
          t1
        } else {
          TString
        }
      }
      case t: Equals     => {
        var t1 = tcExpr(t.lhs)
        var t2 = tcExpr(t.rhs)
        if (t1.isSubTypeOf(anyRef) && t2.isSubTypeOf(anyRef)) {
          // Then it's ok, like A = B
        } else if (t1 == t2) {
          // Also ok, like Int = Int
        } else {
          error("can't compare types " + t1 + " and " + t2, t)
        }
        TBoolean
      }
      case t: If         => {
        tcExpr(t.expr, TBoolean)
        var tif = tcExpr(t.thn)
        t.els match {
          case Some(x) => {
            val tel = tcExpr(x)
            leastUpperBoundType(tif, tel) match {
              case TError => { error("Type error: type of if (" + tif + ") does not match type of else (" + tel + ")", t); TError }
              case typ => typ
            }
          }
          case None => { 
            if (tif != TUnit) { error("Type error: type of if must be Unit without else", t); TUntyped }
            else TUnit
          }
        }
      }
      case t: Block      => { 
        t.exprs.foreach(x => tcExpr(x))
        if(t.exprs.size != 0) tcExpr(t.exprs.last) 
        else TUnit
      }
      case t: Assign     => { 
        val q = tcExpr(t.expr);
        if (!q.isSubTypeOf(t.id.getType)) {
          t.expr match {
            case t: Null => if (!q.isSubTypeOf(anyRef)) error("type mismatch: can't assign " + q + " to null", t)
            case _ => error("type mismatch: can't assign " + q + " to " + t.id.getType, t)
          }
        }
        TUnit 
      }
      case t: MethodCall => {
        // Check the type of the object
        tcExpr(t.obj) match {
          case TAnyRef(cs) => cs.lookupMethod(t.meth.value) match {
            // It was a class, so that's ok. Now look up the correct method in the ref'd class.
            case Some(x) => {
              // We found a method with the correct name, good!
              t.meth.setSymbol(x) // Link to correct method
              if (t.args.size != x.params.size) {
                error("wrong number of arguments for method call, expected " + x.params.size, t.meth)
              }
              // Match every argument type and check that the parameter list has the same types
              (t.args, x.argList).zipped.foreach((a, b) => {
                tcExpr(a, b.getType)
              })

              x.getType
            }
            case None => {
              error("function " + t.meth.value + " is not a member of " + t.obj.getType, t.meth)
              TUnit
            }
          }
          case te => {
            error("cannot call method on " + te, expr) // Primitive types
            TUnit
          }
        }
      }
      case t: True       =>   TBoolean
      case t: False      =>   TBoolean
      case t: IntLit     =>   TInt
      case t: StringLit  =>   TString
      case t: Null       =>   TNull
      case t: New        =>   t.tpe.getType
      case t: This       =>   t.getType 
      case t: Identifier =>   t.getType
      case t: Println    => { tcExpr(t.expr, TString, TInt, TBoolean); TUnit }
      case t: While      => { tcExpr(t.cond, TBoolean); tcExpr(t.body, TUnit); TUnit }
      case t: And        => { tcExpr(t.lhs, TBoolean); tcExpr(t.rhs, TBoolean); TBoolean }
      case t: Or         => { tcExpr(t.lhs, TBoolean); tcExpr(t.rhs, TBoolean); TBoolean }
      case t: Div        => { tcExpr(t.lhs, TInt);     tcExpr(t.rhs, TInt);     TInt }
      case t: Times      => { tcExpr(t.lhs, TInt);     tcExpr(t.rhs, TInt);     TInt }
      case t: Minus      => { tcExpr(t.lhs, TInt);     tcExpr(t.rhs, TInt);     TInt }
      case t: LessThan   => { tcExpr(t.lhs, TInt);     tcExpr(t.rhs, TInt);     TBoolean }
      case t: Not        => { tcExpr(t.expr, TBoolean);                         TBoolean }
    }
    expr.setType(tpe)

    // Check result and return a valid type in case of error
    if (expected.isEmpty) {
      tpe
    } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
      error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
      expected.head
    } else {
      tpe
    }
  }

  def leastUpperBoundType(tif: Type, tel: Type): Type = {
    if (tif == tel) {
      return tif;
    }
    if(!tif.isSubTypeOf(anyRef) || !tel.isSubTypeOf(anyRef)) {
      return TError
    }
    if (tif.isSubTypeOf(tel)) {
      return tel
    }
    if (tel.isSubTypeOf(tif)) {
      return tif
    }
    tif match {
      case tif: TAnyRef => tif.classSymbol.parent match {
        case Some(x) => leastUpperBoundType(x.getType, tel)
        case None => return anyRef
      }
      case _ => return TUntyped
    }
  }

  def findFreeVariables(f: FunctionDecl, sym: FunctionSymbol): List[Identifier] = {
    def find(e: ExprTree): List[Identifier] = {
      e match {
        case t: Block      => t.exprs.flatMap(find)
        case t: New        => find(t.tpe)
        case t: Not        => find(t.expr)
        case t: Println    => find(t.expr)
        case t: And        => find(t.lhs)  ::: find(t.rhs)
        case t: Or         => find(t.lhs)  ::: find(t.rhs)
        case t: Plus       => find(t.lhs)  ::: find(t.rhs)
        case t: Minus      => find(t.lhs)  ::: find(t.rhs)
        case t: Times      => find(t.lhs)  ::: find(t.rhs)
        case t: Div        => find(t.lhs)  ::: find(t.rhs)
        case t: LessThan   => find(t.lhs)  ::: find(t.rhs)
        case t: Equals     => find(t.lhs)  ::: find(t.rhs)
        case t: Assign     => find(t.id)   ::: find(t.expr)
        case t: MethodCall => find(t.obj)  ::: t.args.flatMap(find)
        case t: While      => find(t.cond) ::: find(t.body)
        case t: If         => find(t.expr) ::: find(t.thn) ::: t.els.map(find).getOrElse(List())
        case t: IntLit     => List()
        case t: StringLit  => List()
        case t: True       => List()
        case t: False      => List()
        case t: This       => List()
        case t: Null       => List()
        case t: Identifier => sym.params.get(t.value) match {
          case None        => t.getSymbol match {
            case y: VarSymbol => y.isFree = true; List(t)
            case _ => List(t)
          } 
          case Some(x)     => List()
        }
        case t: FunctionCall => t.meth match {
          case r: Identifier => find(r)
          case r: FunctionCall => find(r.meth)
          case q => find(q)
        }
        case t: FunctionDecl => List()
      }
    }

    find(f.expr)
  }
}
