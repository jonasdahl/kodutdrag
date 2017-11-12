package punkt0
package code

import Constants._
import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._

object CodeHelper {
	// To generate unique labels for goto statements, labelIndex is used
	// Should be incremented each time a new label is created, using newLabel()
	object LabelID {
		var c: Int = 0

		def next: String = {
			c = c + 1
			"label" + c
		}
	}

	// Returns a string with the class name the the given type can be cast to
	def castify(t: Type): String = t match {
		case TInt => INTWRAPPER
		case TBoolean => BOOLWRAPPER
		case TString => "java/lang/String"
		case t: TAnyRef => t.toString
		case t: TFunction => "Function$" + t.args.size
		case _ => ""
	}

	// Returns method signature for JVM/Cafebabe as string
	def getMethodSignature(mt: Type, m: String): String = mt match {
		case t: TAnyRef => {
			val methSym = t.classSymbol.lookupMethod(m).get
			"(" + methSym.argList.map(x => getTypeString(x.getType)).mkString + ")" + getTypeString(methSym.getType)
		}
		case _ => sys.error("Internal error")
	}

	// Returns the type string for return statements and arguments
	def getTypeString(tpe: Type): String = {
		val rt = tpe match {
			case TUnit        => "V"
			case TBoolean     => "Z"
			case TInt         => "I"
			case TString      => o(STRING)
			case t: TAnyRef   => o(t.toString)
			case t: TFunction => o(FUNCTION + t.args.size)
			case _            => sys.error("Internal compiler error")
		}
		rt
	}

	// Surrounds the given string with L and ;. A sample usage is for example 
	// converting class names to JVM class names java/lang/String --> Ljava/lang/String;
	def o(x: String) = "L" + x + ";"
}