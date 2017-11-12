package punkt0

import java.io.File
import java.io._

import lexer._
import ast._
import analyzer._
import code._
import sys.process._

object Main {
  // Processes list of arguments and returns the context to which these arguments corresponds
  // If help or test is enabled, calling this method will result in sys.exit(0)
  def processOptions(args: Array[String]): Context = {
    var ctx = Context()

    def processOption(args: List[String]): Unit = args match {
      case "--test" :: lab :: args =>
        ctx = ctx.copy(test = lab.toInt)
        processOption(args)
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)
      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)
      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)
      case "--print" :: args =>
        ctx = ctx.copy(doPrintMain = true)
        processOption(args)
      case "--eval" :: args =>
        ctx = ctx.copy(doEval = true)
        processOption(args)
      case "--ast" :: args =>
        ctx = ctx.copy(doAST = true)
        processOption(args)
      case "--symid" :: args =>
        ctx = ctx.copy(doSymbolIds = true)
        processOption(args)
      case f :: args =>
        ctx = ctx.copy(file = Some(new File(f)))
        processOption(args)
      case List() => 
    }

    processOption(args.toList)

    ctx
  }

  // Prints the usage of the compiler to stdout
  def displayHelp(): Unit = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" -d <outdir>   generates class files in the specified directory")
    println(" --help        displays this help")
    println(" --tokens      prints tokens in file")
    println(" --print       prettyprints file")
    println(" --ast         prettyprints ast of program")
    println(" --eval        lexes, parses, name analyses and type checks")
    println(" --symid       prints symbol ids after each identifier")
    println(" --test <lab>  runs tests for lab 3 or 5")
  }

  // Parses options, and do what we're told
  def main(args: Array[String]): Unit = {
    // Init context, process options
    val ctx = processOptions(args)

    if (ctx.doHelp) {
      displayHelp()
      return
    }

    if (ctx.test != 0) {
      test(ctx)
      return
    }

    // Always do the lexical analysis
    val lexer = Lexer.run(ctx.file.get)(ctx)
    if (ctx.doTokens) { lexer.foreach(x => println(x + x.posString)); return }

    // Parse program
    var program = Parser.run(lexer)(ctx)
    if (ctx.doAST) { println(program); return }
    if (ctx.doPrintMain) { println(Printer.apply(program, 0, ctx)); return }

    // Name analyze
    program = NameAnalysis.run(program)(ctx)

    // Check types
    program = TypeChecking.run(program)(ctx)
    if (ctx.doSymbolIds) { println(Printer.apply(program, 0, ctx)); return }
    if (ctx.doEval) { println("Program OK."); return }

    // Generate the code
    CodeGeneration.run(program)(ctx)
  }

  // Test method for testing the different labs
  // Uses the context test variable to determine test
  // Only labs 3 and 5 can be tested, other tests will terminate program
  def test(c: Context): Unit = {
    val ctx = c.copy(file = Some(new File("test")))
    var validTests = ctx.test match {
      case 3 | 5 => List("99bottles.p0", "ComplexNumbers.p0", "GCD.p0", "Multiplicator.p0", "Polymorphism.p0", "ScalarProduct.p0", "VehicleRent.p0", "BinarySearch.p0", "DrawStuff.p0", "HeapSort.p0", "NewtonsMethod.p0", "PrimeTest.p0", "Simple.p0", "Calendar.p0", "Factorial.p0", "Life.p0", "OptimalChange.p0", "QuickSort.p0", "Sudoku.p0")
      case 7 => List("EasyLambda.p0", "FreeFlying.p0", "References.p0", "Filter.p0", "NumberPrinter.p0", "StringPrinter.p0")
      case _ => println("[***] Hittade inga valid tester för lab " + ctx.test); return
    }
    var invalidTests = ctx.test match {
      case 3 | 5 => List("App.p0", "MethodCall.p0", "OverloadedPlus2.p0", "Override2.p0", "Override4.p0", "Return.p0", "Comparison.p0", "OverloadedPlus.p0", "Override1.p0", "Override3.p0", "Println.p0")
      case 7 => List()
      case _ => { println("[***] Hittade inga invalid tester för lab " + ctx.test); return }
    }

    println("[***] Kör valid tests för lab " + ctx.test + " och ser om de kompilerar")
    validTests.foreach(test => {
      print("      Kör test " + test)

      var file = Some(new File("testprograms/lab" + ctx.test + "/valid/" + test))
      var lexer = Lexer.run(file.get)(ctx)
      var program = Parser.run(lexer)(ctx)
      program = NameAnalysis.run(program)(ctx)
      program = TypeChecking.run(program)(ctx)
      CodeGeneration.run(program)(ctx)

      if (ctx.test == 7) {
        val result = "java Main"!!
        val correctFile = Some(new File("testprograms/lab" + ctx.test + "/valid/" + test + ".out"))
        val correct = scala.io.Source.fromFile(correctFile.get).mkString
        if (result.trim != correct.trim) {
          println((" " * (20 - test.length)) + "INTE OK - fel output!")
          println()
          println("Fick: ")
          println(result)
          println()
          println("Istället för:")
          println(correct)
          return
        }
      }
      println((" " * (20 - test.length)) + "OK!")
    })


    println("[***] Kör invalid tests för lab " + ctx.test)
    invalidTests.foreach(test => {
      print("      Kör test " + test)
      try {
        var file = Some(new File("testprograms/lab" + ctx.test + "/valid/" + test))
        var lexer = Lexer.run(file.get)(ctx)
        var program = Parser.run(lexer)(ctx)
        program = NameAnalysis.run(program)(ctx)
        program = TypeChecking.run(program)(ctx)
        println((" " * (20 - test.length)) + "INTE OK! Kastade inget exception!")
        } catch {
          case e: Exception => println((" " * (20 - test.length)) + "OK!")
        }
        })

    if (ctx.test == 3) {
      println("[***] Testar att parsea en gång och jämföra med deras AST")
      println("      Här har några testfiler ändrats vilket resulterar i felaktiga \"INTE OK :(((\".")
      val tests = List("99bottles.p0", "ComplexNumbers.p0", "GCD.p0", "Multiplicator.p0", "Polymorphism.p0", "ScalarProduct.p0", "VehicleRent.p0", "BinarySearch.p0", "DrawStuff.p0", "HeapSort.p0", "NewtonsMethod.p0", "PrimeTest.p0", "Simple.p0", "Calendar.p0", "Factorial.p0", "Life.p0", "OptimalChange.p0", "QuickSort.p0", "Sudoku.p0")
      tests.foreach(test => {
        print("      Testar " + test + " " * (20 - test.length))
        val file = Some(new File("testprograms/lab3/valid/" + test))
        val correctFile = Some(new File("testprograms/lab3/valid/" + test + ".ast"))
        val lexer = Lexer.run(file.get)(ctx)
        var program = Parser.run(lexer)(ctx)
        program = NameAnalysis.run(program)(ctx)
        program = TypeChecking.run(program)(ctx)
        val correctProgram = scala.io.Source.fromFile(correctFile.get).mkString

        if (correctProgram.trim == program.toString.trim) {
          println("OK!")
        } else {
            println("INTE OK")
        }
      })

      println("[***] Testar att parsea en och tvÃ¥ gÃ¥nger och jÃ¤mföra AST för de testerna")
      tests.foreach(test => {
        print("      Testar " + test + " " * (20 - test.length))
        val file = Some(new File("testprograms/lab3/valid/" + test))

        val lexer1 = Lexer.run(file.get)(ctx)
        val program1 = Parser.run(lexer1)(ctx)

        new PrintWriter("/tmp/test") { write(Printer.apply(program1, 0, ctx)); close }
        val file2 = Some(new File("/tmp/test"))
        val lexer2 = Lexer.run(file2.get)(ctx)
        val program2 = Parser.run(lexer2)(ctx)

        if (program1.toString.trim == program2.toString.trim) {
          println("OK!")
          } else {
            println("INTE OK :((((((((")
            println()
            println("Output 1:")
            println(program1)
            println()
            println("Output 2:")
            println(program2)
            return
          }
        }
        )
    }
  }
}