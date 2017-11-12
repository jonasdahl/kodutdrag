package punkt0
package ast

import Trees._
import analyzer.Symbols._
import analyzer.Types._

object Printer {
  def apply(t: Tree, indent: Int, c: Context): String = {
    def op(lhs: Tree, rhs: Tree, op: String): String = "(" + Printer.apply(lhs, indent, c) + " " + op + " " + Printer.apply(rhs, indent, c) + ")"
    var x = t.posString
    var y = t match {
        case t: Program => t.classes.map(cl => Printer.apply(cl, indent, c) + "\n\n").mkString + Printer.apply(t.main, indent, c)
        case t: ClassDecl => {
            var str = "\t" * indent + "class " + Printer.apply(t.id, indent, c) 
            str = str + (if (t.parent != None) " extends " + Printer.apply(t.parent.get, indent, c) else "")
            str = str + " {" + "\n"
            str = str + t.vars.map(v => Printer.apply(v, indent + 1, c)).mkString + (if (t.vars.size > 0) "\n" else "")
            str = str + t.methods.map(m => Printer.apply(m, indent + 1, c)).mkString("\n\n")
            str + "\n" + "\t" * indent + "}"
        }
        case t: MethodDecl => {
            var str = "\t" * indent
            str = str + (if (t.overrides) "override " else "")
            str = str + "def " + Printer.apply(t.id, indent, c)
            str = str + "(" + t.args.map(arg => Printer.apply(arg.id, indent, c) + ": " + Printer.apply(arg.tpe, indent, c)).mkString(", ")
            str = str + "): " + Printer.apply(t.retType, indent, c) + " = {\n"
            str = str + t.vars.map(v => Printer.apply(v, indent + 1, c)).mkString
            str = str + t.exprs.map(e => "\t" * (indent + 1) + Printer.apply(e, indent, c) + ";\n").mkString
            str = str + "\t" * (indent + 1) + Printer.apply(t.retExpr, indent, c)
            str + "\n" + "\t" * indent + "}"
        }
    	case t: MainDecl => {
    		var str = "\t" * indent + "object " + Printer.apply(t.obj, indent, c) 
            str = str + " extends " + Printer.apply(t.parent, indent, c)
            str = str + " {" + "\n"
            str = str + t.vars.map(v => Printer.apply(v, indent + 1, c)).mkString
            str = str + t.exprs.map(e => "\t" * (indent + 1) + Printer.apply(e, indent + 1, c)).mkString(";\n")
    		str + "\n" + "\t" * indent + "}\n\n"
    	}
    	case t: VarDecl => {
    		var str = "\t" * indent + "var " + Printer.apply(t.id, indent, c) 
            str + ": " + Printer.apply(t.tpe, indent, c) + " = " + Printer.apply(t.expr, indent, c) + ";\n"
    	}
        case t: FunctionCall => {
            var str = Printer.apply(t.meth, indent, c) + "("
            str + t.args.map(arg => Printer.apply(arg, indent, c)).mkString(", ") + ")"
        }
        case t: MethodCall => {
            var str = Printer.apply(t.obj, indent, c) + "." + Printer.apply(t.meth, indent, c) + "("
            str + t.args.map(arg => Printer.apply(arg, indent, c)).mkString(", ") + ")"
        }
        case t: FunctionDecl => {
            var str = ""
            str = str + "(" + t.args.map(arg => Printer.apply(arg, indent, c)).mkString(", ")
            str = str + ") => " + Printer.apply(t.expr, indent, c)
            str
        }
        case t: If => {
            var str = "if (" + Printer.apply(t.expr, indent, c) + ") " + Printer.apply(t.thn, indent + 1, c)
            str + (if (t.els != None) " else " + Printer.apply(t.els.get, indent + 1, c) else "")
        }
    	case t: BooleanType => "Boolean"
    	case t: StringType  => "String"
    	case t: IntType     => "Int"
        case t: UnitType    => "Unit"
        case t: FunctionType=> (if (t.args.size != 1 || t.args.head.isInstanceOf[FunctionType]) "(" else "") + t.args.map(Printer.apply(_, indent, c)).mkString(", ") + (if (t.args.size != 1 || t.args.head.isInstanceOf[FunctionType]) ")" else "") + " => " + Printer.apply(t.ret, indent, c)
    	case t: Identifier  => t.value + (if(c.doSymbolIds) "#" + t.getSymbol.id else "")
    	case t: StringLit   => "\"" + t.value + "\""
    	case t: True        => "true"
    	case t: False       => "false"
    	case t: This        => "this" + (if (c.doSymbolIds) "#" + t.getSymbol.id else "")
    	case t: Null        => "null"
    	case t: New         => "new " + Printer.apply(t.tpe, indent, c) + "()"
    	case t: Assign      => Printer.apply(t.id, indent, c) + " = " + Printer.apply(t.expr, indent, c)
        case t: And         => op(t.lhs, t.rhs, "&&")
        case t: Or          => op(t.lhs, t.rhs, "||")
        case t: Plus        => op(t.lhs, t.rhs, "+")
        case t: Minus       => op(t.lhs, t.rhs, "-")
        case t: Times       => op(t.lhs, t.rhs, "*")
        case t: Div         => op(t.lhs, t.rhs, "/")
        case t: LessThan    => op(t.lhs, t.rhs, "<")
        case t: Equals      => op(t.lhs, t.rhs, "==")
        case t: Not         => "!(" + Printer.apply(t.expr, indent, c) + ")"
        case t: Block       => "{\n" + "\t" * (indent + 1) + t.exprs.map(e => Printer.apply(e, indent + 1, c)).mkString(";\n" + "\t" * (indent + 1)) + "\n" + "\t" * indent + "}"
        case t: While       => "while (" + Printer.apply(t.cond, indent, c) + ") " + Printer.apply(t.body, indent + 1, c)
        case t: Println     => "println(" + Printer.apply(t.expr, indent, c) + ")"
        case t: IntLit      => "" + t.value
        case t: Formal      => Printer.apply(t.id, indent, c) + ": " + Printer.apply(t.tpe, indent, c)
    }

    if (c.doSymbolIds && false) {
        // Add type
        var z = t match {
            case t: Symbolic[_] => t.getSymbol match {
                case cs: ClassSymbol    => "[[" + cs.getType + "]]"
                case ms: MethodSymbol   => "[[" + ms.getType + "]]"
                case vs: VariableSymbol => "[[" + vs.getType + "]]"
                case fs: FunctionSymbol => "[[" + fs.getType + "]]"
                case _ => "FEL"
            }
            case t: ExprTree => "[[" + t.getType + "]]"
            case _ => ""
        }
        y + z
    } else {
        y
    }
  }
}
