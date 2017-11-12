package punkt0

import java.io.File

case class Context(
  file: Option[File] = None, 	// Argument
  outDir: Option[File] = None, 	// -d
  doEval: Boolean = false, 		// ?
  doHelp: Boolean = false, 		// --help
  doPrintMain: Boolean = false, // --print
  doTokens: Boolean = false, 	// --tokens
  doAST: Boolean = false, 		// --ast
  doSymbolIds: Boolean = false, // --symid
  test: Int = 0 				// --test
)
