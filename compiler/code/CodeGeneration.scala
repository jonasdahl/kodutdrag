package punkt0
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import cafebabe.Flags._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import java.io.IOException
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import Constants._
import CodeHelper._

/**
 * The code generation class takes a program abstract syntax tree and outputs JVM class
 * files representing the program. 
 * Lambda functions are represented by objects of classes extending the Function$<n> classes, 
 * where <n> is the number of parameters taken by the lambda function. These objects overrides
 * the apply(...) methods of Function$.., which is called on a function call. Variables that
 * are not sent as parameters, but used inside the lambda functions, are wrapped in an outer 
 * class, both in the "mother" defining class, which shares the reference of the wrapping class
 * with the lambda function. This means that changes made to the variable in the lambda function
 * will persist, in both directions. However, since non-free varables are treated as primitive 
 * data types when possible, this leads to a couple of interesting situations that needs to be
 * taken into account. When using wrapped values, they first need to be unwrapped. When writing
 * to them, they should be written directly to the field in the wrapper.
 */
object CodeGeneration extends Phase[Program, Unit] {
  // Generates code into specified files in dir specified by -d in context
  def run(prog: Program)(ctx: Context): Unit = {
    // Get the wanted outdir and filename
    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")
    val sourceName = ctx.file.get.getName

    // Create file if not exists
    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    // Stores a set of ints, where if an int is in the set represents
    // that we need to generate a anonymous function interface for it
    var fToGen = Set[Int]()

    // Output code
    prog.classes.foreach(generateClassFile)
    generateMainFile(prog.main)

    generateWrapper(INTWRAPPER, "I")
    generateWrapper(BOOLWRAPPER, "Z")
    generateWrapper(OBJWRAPPER, o(OBJECT))
    fToGen.foreach(generateFunction)
    generateConverters

    // End of code, below are only function calls
    // The beaty is at the bottom
    // The following functions are used to generate the class files

    // Generates a wrapper with a field containing the wrapped stuff
    // Needs name and type, and even constructs a toString method returning the
    // String representation of the value, which makes it possible to send
    // directly to println
    def generateWrapper(name: String, typeString: String) {
      val classFile = new ClassFile(name)
      classFile.addDefaultConstructor
      classFile.setSourceFile(sourceName)
      classFile.addField(typeString, WRAPPERFIELD)
      val ch = classFile.addMethod(o(STRING), "toString", "").codeHandler
      ch << DefaultNew(STRINGBUILDER) << ALOAD_0 << GetField(name, WRAPPERFIELD, typeString)
      ch << InvokeVirtual(STRINGBUILDER, "append", "(" + typeString + ")" + o(STRINGBUILDER))
      ch << InvokeVirtual(STRINGBUILDER, "toString", "()" + o(STRING))
      ch << ARETURN
      ch.freeze
      classFile.writeToFile(outDir + name + CLASS_EXT)
    }

    // Sometimes we need to get the actual values of stuff 
    // in their containing class
    // We use the toInteger (int -> Integer), toInt (Integer -> int)
    // toBoolean (boolean -> Boolean) and toBool (Boolean -> boolean) for this
    def generateConverters {
      val classFile = new ClassFile(WRAP)
      classFile.addDefaultConstructor
      classFile.setSourceFile(sourceName)

      genSetter(o(OBJECT), TO_INT_OBJECT,  "I", INTWRAPPER)
      genSetter(o(OBJECT), TO_BOOL_OBJECT, "Z", BOOLWRAPPER)
      genSetter(o(OBJECT), TO_OBJ_OBJECT,  o(OBJECT), OBJWRAPPER)

      genGetter("I", FROM_INT_OBJECT,  o(OBJECT), INTWRAPPER)
      genGetter("Z", FROM_BOOL_OBJECT, o(OBJECT), BOOLWRAPPER)
      genGetter(o(OBJECT), FROM_OBJ_OBJECT, o(OBJECT), OBJWRAPPER)

      // Generates a setter for the value field of the given types and names
      def genSetter(ret: String, name: String, arg: String, cls: String) {
        val mh = classFile.addMethod(ret, name, arg)
        mh.setFlags(METHOD_ACC_STATIC)
        val ch = mh.codeHandler
        val id = ch.getFreshVar
        ch << DefaultNew(cls) << AStore(id) << ALoad(id) << (if (arg != o(OBJECT)) ILOAD_0 else ALOAD_0) << PutField(cls, WRAPPERFIELD, arg) << ALoad(id) << ARETURN
        ch.freeze
      }

      // Generates a getter for the value field of the given types and names
      def genGetter(ret: String, name: String, args: String, cls: String) {
        val mh = classFile.addMethod(ret, name, args)
        mh.setFlags(METHOD_ACC_STATIC)
        val ch = mh.codeHandler
        ch << ALOAD_0 << CheckCast(cls) << GetField(cls, WRAPPERFIELD, ret) << (if (ret != o(OBJECT)) IRETURN else ARETURN)
        ch.freeze
      }

      classFile.writeToFile(outDir + WRAP + CLASS_EXT)
    }

    // Generates a function class. These always looks the same. However, they are compiled
    // based on which functions are used. If only 2-param functions are used, only
    // Function$2 should be compiled, since the others are unnecessary. Creates an empty
    // apply function since it is hard to do abstract methods in cafebabe.
    def generateFunction(i: Int) {
      val classFile = new ClassFile(FUNCTION + i)
      classFile.setFlags(CLASS_ACC_ABSTRACT)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor
      val mh = classFile.addMethod(o(OBJECT), APPLY_FUNCTION, o(OBJECT) * i)
      mh.setFlags(METHOD_ACC_PUBLIC)
      val ch = mh.codeHandler
      ch << ACONST_NULL << ARETURN // Should be overridden anyway
      ch.freeze
      classFile.writeToFile(outDir + FUNCTION + i + CLASS_EXT)
    }

    // Generates a lambda function class as described in 
    // the comments above this class declaration
    def generateAnonymousFunctionClass(fd: FunctionDecl) {
      val classFile = new ClassFile(fd.getSymbol.className, Some(FUNCTION + fd.args.size))

      // Add fields for all free variables
      fd.getSymbol.freeVariables.foreach({ case (a, b) => classFile.addField(o(OBJECT), b) })
      classFile.addDefaultConstructor

      // Define the apply method
      // The stack variables are the parameters, the compile function will make
      // sure to update the correct variable (free/non-free)
      val ch = classFile.addMethod(o(OBJECT), APPLY_FUNCTION, o(OBJECT) * fd.args.size).codeHandler
      var stackVars = fd.args.map(_.id.value).zip(Stream from 1).toMap
      compileExpression(stackVars, ch, fd.expr, Some(fd.getSymbol.getMethodSymbol), Some(fd.getSymbol))
      fd.getType.asInstanceOf[TFunction].ret match {
        case TInt     => ch << InvokeStatic(WRAP, TO_INT_OBJECT,  "(I)" + o(OBJECT))
        case TBoolean => ch << InvokeStatic(WRAP, TO_BOOL_OBJECT, "(Z)" + o(OBJECT))
        case TUnit    => ch << ACONST_NULL
        case t        => ch << InvokeStatic(WRAP, TO_OBJ_OBJECT,  "(" + o(OBJECT) + ")" + o(OBJECT))
      }
      ch << ARETURN
      ch.freeze
      classFile.writeToFile(outDir + fd.getSymbol.className + CLASS_EXT)
    }

    // The starting point of generating a class
    // Writes the proper .class file in a given directory. 
    def generateClassFile(ct: ClassDecl): Unit = {
      // Let's start by creating the class file
      val classFile = new ClassFile(ct.id.value, ct.parent.map(x => x.value))
      classFile.setSourceFile(sourceName)

      // Add place to store the fields
      ct.vars.foreach(v => classFile.addField(getTypeString(v.getSymbol.getType), v.id.value))

      // We add a constructor where we initialize all fields and give them values
      val mh = classFile.addConstructor(Nil)
      val ch = mh.codeHandler
      // Important to call parent's constructor first
      ch << ALOAD_0 << InvokeSpecial(ct.parent.map(_.value).getOrElse(OBJECT), "<init>", "()V")

      // Store every field variable with its init value
      ct.vars.foreach(v => {
        ch << ALOAD_0
        compileExpression(Map[String, Int](), ch, v.expr)
        ch << PutField(ct.id.value, v.id.value, getTypeString(v.getSymbol.getType))
      })
      ch << RETURN
      ch.freeze
      
      // Time to add methods
      ct.methods.foreach(m => {
        val codeHandler = classFile.addMethod(
          getTypeString(m.retType.getType), 
          m.id.value, 
          m.args.map(x => getTypeString(x.tpe.getType)).mkString
        ).codeHandler
        generateMethodCode(codeHandler, m)
      })

      // At last, save to file
      classFile.writeToFile(outDir + ct.id.value + CLASS_EXT)
    }

    // Writes Main.class to file in the proper directory
    // No fields are used in the Main class, the variables are local
    def generateMainFile(md: MainDecl): Unit = {
      // Create the class file and add empty constructor since we do not have our own
      val classFile = new ClassFile("Main", None)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      // Add the code to main
      val ch = classFile.addMainMethod.codeHandler
      val classSym = md.getSymbol

      // Stack vars stores a mapping from a variable name to the slot in the virtual machine
      // Slots:
      // j - variable slot j
      var stackVars: Map[String, Int] = Map[String, Int]()
      classSym.members.keys.foreach(v => stackVars = stackVars + (v -> ch.getFreshVar))

      md.vars.foreach(v => assignVar(stackVars, ch, v.id, v.expr, None))
      md.exprs.foreach(e => {compileExpression(stackVars, ch, e); if(e.getType != TUnit) ch << POP})
      ch << RETURN
      ch.freeze

      // At last, save to file
      classFile.writeToFile(outDir + "Main" + CLASS_EXT)
    }

    // Adds code for given method into codehandler
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // Stack vars stores a mapping from a variable name to the slot in the virtual machine
      // Slots:
      // 0     - this
      // i     - argument 1 <= i <= n
      // i + j - variable slot j
      var stackVars: Map[String, Int] = methSym.argList.map(_.name).zip(Stream from 1).toMap
      methSym.members.keys.foreach(v => stackVars = stackVars + (v -> ch.getFreshVar))

      mt.args.foreach(arg => {
        if (arg.getSymbol.asInstanceOf[VarSymbol].isFree) {
          arg.id.getType match {
            case TInt     => ch << ILoad(stackVars.get(arg.id.value).get) << InvokeStatic(WRAP, TO_INT_OBJECT, "(I)" + o(OBJECT)) << AStore(stackVars.get(arg.id.value).get)
            case TBoolean => ch << ILoad(stackVars.get(arg.id.value).get) << InvokeStatic(WRAP, TO_BOOL_OBJECT, "(Z)" + o(OBJECT)) << AStore(stackVars.get(arg.id.value).get)
            case t => ch << ALoad(stackVars.get(arg.id.value).get) << InvokeStatic(WRAP, TO_OBJ_OBJECT, "(" + o(OBJECT) + ")" + o(OBJECT)) << AStore(stackVars.get(arg.id.value).get)
          }
        }
      })

      // Create all variable declarations and assign their values
      mt.vars.foreach(v => assignVar(stackVars, ch, v.id, v.expr, Some(mt.getSymbol)))

      // Compile expressions. The if statement ensures that values from expressions are popped
      // from stack since they will add up on stack otherwise
      mt.exprs.foreach(e => {compileExpression(stackVars, ch, e, Some(mt.getSymbol)); if(e.getType != TUnit) ch << POP})

      // Compile the return statement
      compileExpression(stackVars, ch, mt.retExpr, Some(mt.getSymbol))

      mt.retExpr.getType match {
        case TInt | TBoolean => ch << IRETURN
        case TString | TAnyRef(_) | TNull | TFunction(_,_) => ch << ARETURN
        case _ => ch << RETURN
      }

      ch.freeze
    }

    // Assigns the value on the stack to the slot for the given Identifier
    // This should only be called if we have ensured that we are not assigning to a field!
    // First looks if the Id is a free variable, then we should convert it to object
    // we can the store it.
    def assignSlot(stv: Map[String, Int], ch: CodeHandler, id: Identifier) = {
      if (id.getSymbol.asInstanceOf[VarSymbol].isFree) {
        id.getType match { 
          case TInt =>     ch << InvokeStatic(WRAP, TO_INT_OBJECT, "(I)" + o(OBJECT))
          case TBoolean => ch << InvokeStatic(WRAP, TO_BOOL_OBJECT, "(Z)" + o(OBJECT))
          case t => ch << InvokeStatic(WRAP, TO_OBJ_OBJECT, "(" + o(OBJECT) + ")" + o(OBJECT))
        }
        ch << AStore(stv.get(id.value).get)
      } else {
        id.getType match { 
          case TInt | TBoolean => ch << IStore(stv.get(id.value).get)
          case t => ch << AStore(stv.get(id.value).get)
        }
      }
    }

    // Handles assignment of expr to id
    // id must either be a member, parameter or field
    // We need to check if Id refers to a field or to a local variable. We also need to look if 
    // we are in a lambda function or not since the free variables then will be fields
    def assignVar(stv: Map[String, Int], ch: CodeHandler, id: Identifier, expr: ExprTree, methSym: Option[MethodSymbol] = None, funcSym: Option[FunctionSymbol] = None): Unit = {
      def ordinaryAssignment = {
        compileExpression(stv, ch, expr, methSym, funcSym)
        assignSlot(stv, ch, id)
      }

      funcSym match {
        case Some(fs) => fs.freeVariables.get(id) match { // We're in a function
          case Some(v) => { // We're dealing with a variable that is defined outside the function
            ch << ALOAD_0 // Load local object to be able to get field
            ch << GetField(fs.className, v, o(OBJECT)) // Get the field to update
            ch << CheckCast(castify(id.getType)) // For JVM to be sure there is no type error
            compileExpression(stv, ch, expr, methSym, funcSym) // Compile the value
            ch << PutField(castify(id.getType), WRAPPERFIELD, "I") // Only update the content, not the whole object, to save reference
          }
          case _ => ordinaryAssignment // Just compile and assign as usual if not free
        }
        case _ => methSym match { // Here we differ on inside main and in class
          case Some(ms) => ms.members.get(id.value) match { // Not in main
            case Some(vs) => ordinaryAssignment // It was a member of method, do as usual
            case None => ms.params.get(id.value) match { // It was not a member
              case Some(vs) => ordinaryAssignment // It was a parameter
              case None => { // It must be a class field
                ch << ALOAD_0;
                compileExpression(stv, ch, expr, methSym, funcSym)
                ch << PutField(ms.classSymbol.name, id.value, getTypeString(id.getType))
              }
            }
          }
          case None => ordinaryAssignment // In Main, we always use locals
        }
      }
    }

    // Compiles expressions. Most expressions puts values on the stack in the end.
    def compileExpression(stv: Map[String, Int], ch: CodeHandler, e: ExprTree, methSym: Option[MethodSymbol] = None, funcSym: Option[FunctionSymbol] = None): Unit = {
      compile(e)

      // Loads variable of the identifier onto the stack
      // If the convert flag is true the identifier is converted from wrapping type to raw type
      def loadVar(i: Identifier, convert: Boolean) {
        def push = {
          i.getType match { 
            case TInt     => ch << ALoad(stv.get(i.value).get); if (convert) ch << InvokeStatic(WRAP, FROM_INT_OBJECT, "(" + o(OBJECT) + ")I")
            case TBoolean => ch << ALoad(stv.get(i.value).get); if (convert) ch << InvokeStatic(WRAP, FROM_BOOL_OBJECT, "(" + o(OBJECT) + ")Z")
            case typ      => ch << ALoad(stv.get(i.value).get); if (convert) ch << InvokeStatic(WRAP, FROM_OBJ_OBJECT, "(" + o(OBJECT) + ")" + o(OBJECT) + "") << CheckCast(castify(typ))
          }
        }

        funcSym match {
          case Some(fs) => fs.freeVariables.get(i) match {
            case Some(x) => {
              // i is a free variable with field name x
              i.getType match { 
                case TInt     => ch << ALOAD_0 << GetField(fs.className, x, o(OBJECT)); if (convert) ch << InvokeStatic(WRAP, FROM_INT_OBJECT, "(" + o(OBJECT) + ")I")
                case TBoolean => ch << ALOAD_0 << GetField(fs.className, x, o(OBJECT)); if (convert) ch << InvokeStatic(WRAP, FROM_BOOL_OBJECT,"(" + o(OBJECT) + ")Z")
                case typ      => ch << ALOAD_0 << GetField(fs.className, x, o(OBJECT)); if (convert) ch << InvokeStatic(WRAP, FROM_OBJ_OBJECT, "(" + o(OBJECT) + ")" + o(OBJECT) + "") << CheckCast(castify(typ))
              }
              return
            }
            case _ => {
              // Not a free variable
              push
              return
            }
          }
          case _ => 
        }
        
        if (i.getSymbol.asInstanceOf[VarSymbol].isFree) {
          push
        } else {
          i.getType match { 
            case TInt     => ch << ILoad(stv.get(i.value).get);
            case TBoolean => ch << ILoad(stv.get(i.value).get);
            case _        => ch << ALoad(stv.get(i.value).get);
          }
        }
      }

      // The cool compiling happens here
      def compile(e: ExprTree, convert: Boolean = true): Unit = {
        e match {
          case Minus(e1, e2)    => compile(e1); compile(e2); ch << ISUB
          case Times(e1, e2)    => compile(e1); compile(e2); ch << IMUL
          case Div(e1, e2)      => compile(e1); compile(e2); ch << IDIV
          case Assign(id, expr) => assignVar(stv, ch, id, expr, methSym, funcSym)
          case e: New           => ch << DefaultNew(e.tpe.value)
          case IntLit(v)        => ch << Ldc(v)
          case StringLit(s)     => ch << Ldc(s)
          case True()           => ch << Ldc(1)
          case False()          => ch << Ldc(0)
          case This()           => ch << ALOAD_0
          case Null()           => ch << ACONST_NULL
          case i: Identifier    => funcSym match {
            case Some(fs) => fs.params.get(i.value) match {
              case Some(vs) => loadVar(i, convert) // It was a parameter to the function
              case None => { // It is a free variable
                ch << ALOAD_0;
                ch << GetField(fs.className, fs.freeVariables.get(i).getOrElse({println(i);println(fs);sys.exit()}), "" + o(OBJECT) + "")
                if (convert) i.getType match { 
                  case TInt     => ch << InvokeStatic(WRAP, FROM_INT_OBJECT, "(" + o(OBJECT) + ")I")
                  case TBoolean => ch << InvokeStatic(WRAP, FROM_BOOL_OBJECT, "(" + o(OBJECT) + ")Z")
                  case typ => ch << InvokeStatic(WRAP, FROM_OBJ_OBJECT, "(" + o(OBJECT) + ")" + o(OBJECT)) << CheckCast(castify(typ))
                }
              }
            }
            case None => methSym match {
              case Some(ms) => { // Not in main
                ms.members.get(i.value) match {
                  case Some(vs) => loadVar(i, convert) // It was a member of method
                  case None => ms.params.get(i.value) match {
                    case Some(vs) => loadVar(i, convert) // It was a parameter
                    case None => { // It must be a class field
                      ch << ALOAD_0;
                      ch << GetField(ms.classSymbol.name, i.value, getTypeString(i.getSymbol.getType))
                    }
                  }
                }
              }
              case None => loadVar(i, convert) // In Main
            }
          }
          case And(e1, e2) => {
            val falseLabel = LabelID.next
            val afterLabel = LabelID.next
            compile(e1)
            ch << Ldc(0) << If_ICmpEq(falseLabel)
            compile(e2)
            ch << Ldc(0) << If_ICmpEq(falseLabel) << Ldc(1) << Goto(afterLabel) << Label(falseLabel) << Ldc(0)  << Label(afterLabel)
          }
          case Or(e1, e2) => {
            val trueLabel = LabelID.next
            val afterLabel = LabelID.next
            compile(e1)
            ch << Ldc(1) << If_ICmpEq(trueLabel)
            compile(e2)
            ch << Ldc(1) << If_ICmpEq(trueLabel) << Ldc(0) << Goto(afterLabel) << Label(trueLabel) << Ldc(1)  << Label(afterLabel)
          }
          case LessThan(e1, e2) => {
            val elseLabel = LabelID.next
            val afterLabel = LabelID.next
            compile(e1); 
            compile(e2);
            ch << If_ICmpLt(elseLabel) << Ldc(0) << Goto(afterLabel) << Label(elseLabel) << Ldc(1) << Label(afterLabel)
          }
          case Equals(e1, e2) => {
            val elseLabel = LabelID.next
            val afterLabel = LabelID.next
            compile(e1)
            compile(e2)
            ch << (e1.getType match {
              case TInt | TBoolean => If_ICmpEq(elseLabel)
              case _ => If_ACmpEq(elseLabel)
            })
            ch << Ldc(0) << Goto(afterLabel) << Label(elseLabel) << Ldc(1) << Label(afterLabel)
          }
          case Not(e) => {
            val elseLabel = LabelID.next
            val afterLabel = LabelID.next
            compile(e) 
            ch << Ldc(1) << If_ICmpEq(elseLabel) << Ldc(1) << Goto(afterLabel) << Label(elseLabel) << Ldc(0) << Label(afterLabel)
          }
          case Plus(e1, e2) => if(e1.getType == TInt && e2.getType == TInt) {
            compile(e1); compile(e2); ch << IADD
          } else {
            buildString(e1, e2)
          }
          case t: MethodCall => {
            compile(t.obj)
            t.args.foreach(x => compile(x))
            ch << InvokeVirtual(t.obj.getType.toString, t.meth.value, getMethodSignature(t.obj.getType, t.meth.value))
          }
          case Println(e) => {
            ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
            buildString(e)
            ch << InvokeVirtual("java/io/PrintStream", "println", "(" + o(STRING) + ")V")
          }
          case Block(exprs) => {
            if (exprs.size > 0) {
              exprs.init.foreach(e => { compileExpression(stv, ch, e, methSym, funcSym); if(e.getType != TUnit) ch << POP })
              compileExpression(stv, ch, exprs.last, methSym, funcSym)
            }
          }
          case If(ifCond, thn, els) => {
            val elseLabel = LabelID.next
            val afterLabel = LabelID.next
            ch << Ldc(0)
            compile(ifCond)
            ch << If_ICmpEq(elseLabel)
            compile(thn)
            ch << Goto(afterLabel) << Label(elseLabel)
            els match {
              case Some(e) => compile(e)
              case None => 
            }
            ch << Label(afterLabel)
          }
          case While(whileCond, stmt) => {
            val beforeLabel = LabelID.next
            val afterLabel = LabelID.next
            ch << Label(beforeLabel) << Ldc(0)
            compile(whileCond)
            ch << If_ICmpEq(afterLabel)
            compile(stmt)
            ch << Goto(beforeLabel) << Label(afterLabel)
          }
          case t: FunctionCall => {
            val tpe = t.meth.getType.asInstanceOf[TFunction]
            compile(t.meth)
            ch << CheckCast(FUNCTION + t.args.size)
            t.args.foreach(x => {
              compile(x)
              x.getType match {
                case TInt     => ch << InvokeStatic(WRAP, TO_INT_OBJECT, "(I)" + o(OBJECT))
                case TBoolean => ch << InvokeStatic(WRAP, TO_BOOL_OBJECT,"(Z)" + o(OBJECT))
                case typ      => ch << InvokeStatic(WRAP, TO_OBJ_OBJECT, "(" + o(OBJECT) + ")" + o(OBJECT))
              }
            })
            ch << InvokeVirtual(FUNCTION + tpe.args.size, APPLY_FUNCTION, "(" + tpe.args.map(x => "" + o(OBJECT) + "").mkString + ")" + o(OBJECT) + "")
            t.getType match {
              case TInt     => ch << InvokeStatic(WRAP, FROM_INT_OBJECT, "(" + o(OBJECT) + ")I")
              case TBoolean => ch << InvokeStatic(WRAP, FROM_BOOL_OBJECT,"(" + o(OBJECT) + ")Z")
              case TUnit    => ch << POP
              case typ      => ch << InvokeStatic(WRAP, FROM_OBJ_OBJECT, "(" + o(OBJECT) + ")" + o(OBJECT)) << CheckCast(castify(typ))
            }
          }
          case t: FunctionDecl => {
            fToGen += t.args.size // Remember to compile the parent class later
            generateAnonymousFunctionClass(t) // Compile the body

            // Create a new instance of the anonymous class
            val containingClass = t.getSymbol.getClassSymbol.name
            val funcClass = t.getSymbol.className
            ch << cafebabe.AbstractByteCodes.New(funcClass) << DUP << InvokeSpecial(funcClass, "<init>", "()V")
            
            val id = ch.getFreshVar
            ch << AStore(id)

            // For every free variable, we need to put the references in the fields
            t.getSymbol.freeVariables.foreach({
              case (a: Identifier, b: String) => {
                ch << ALoad(id)
                compile(a, false)
                ch << PutField(funcClass, b, "" + o(OBJECT) + "")
              }
            })

            ch << ALoad(id)
          }
        }
      }

      // Builds a string from exprs
      def buildString(exprs: ExprTree*) {
        ch << DefaultNew(STRINGBUILDER)
        exprs.foreach (e => {
          compile(e)
          e.getType match {
            case TInt     => ch << InvokeStatic(WRAP, TO_INT_OBJECT, "(I)" + o(OBJECT) + "")
            case TBoolean => ch << InvokeStatic(WRAP, TO_BOOL_OBJECT,"(Z)" + o(OBJECT) + "")
            case _        => ch << InvokeStatic(WRAP, TO_OBJ_OBJECT, "(" + o(OBJECT) + ")" + o(OBJECT) + "")
          }
          ch << InvokeVirtual(STRINGBUILDER, "append", "(" + o(OBJECT) + ")" + o(STRINGBUILDER))
        })
        ch << InvokeVirtual(STRINGBUILDER, "toString", "()" + o(STRING))
      }
    }

    def beauty() {
      println("This is beautiful.")
      sys.exit(0)
    }
  }
}
