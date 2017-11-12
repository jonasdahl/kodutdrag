package punkt0
package analyzer

import Symbols._

object Types {

  trait Typed {
    private var _tpe: Type = TUntyped

    def setType(tpe: Type): this.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
    def sign: String
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
    override def sign = "ERR"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
    override def sign = "NONE"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "Int"
    override def sign = "I"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString = "Boolean"
    override def sign = "Z"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "String"
    override def sign = "STR"
  }

  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case _ => false
    }
    override def toString = "Unit"
    override def sign = "V"
  }

  case object TNull extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case t: TAnyRef => true
      case TNull => true
      case _ => false
    }
    override def toString = "Null"
    override def sign = "NULL"
  }

  case class TFunction(args: List[Type], ret: Type) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case x: TFunction => args.size == x.args.size &&
        args.zip(x.args).map({
          case (a, b) => a.isSubTypeOf(b)
        }).forall(_ == true) && ret.isSubTypeOf(x.ret)
      case _ => false
    }

    override def sign = args.map(_.sign).mkString + "$" + ret.sign

    override def toString = "(" + args.map(_.toString).mkString(", ") + ") => " + ret
  }

  case class TAnyRef(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = {
      if (tpe == classSymbol.getType || tpe == anyRef || tpe == TUnit) {
        return true
      }
      classSymbol.parent match {
        case Some(x) => x.getType.isSubTypeOf(tpe)
        case None => false
      }
    }
    override def toString = classSymbol.name
    override def sign = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TAnyRef(new ClassSymbol("AnyRef"))
}
