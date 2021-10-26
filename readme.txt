Source files:
- ImpAST.hs: defines AST and Formula types. Pretty-print to Impp and C formats.
- ImpFixpoint.hs: computes postcondition and invariant from a constraint-abstraction
- ImpFormula.hs: the only file that imports InSolver
  exports equate,rename,apply,noChange,composition,ctxImplication,simplify,ctxSimplify
- ImpLexer.hs, ImpParser.y
- ImpMain.hs: type inference or type checking (flag "-c")
- ImpSugar.hs: desugaring + specialization
- ImpTypeCommon.hs: defines type environment + common features from ImpTypeChecker and ImpTypeInfer
- ImpTypeChecker.hs: type checking
- ImpTypeInfer.hs: type inference + groups functions in StronglyConnectedComponents
- InSolver.hs: the only file that imports Razvan's Omega files (converts ImpFormulae to OmegaFormulae and back)
  exports impSubset,impSimplify,impGist,impHull,impUnion,impCompose


Other files:
- Fresh.hs: defines FS Monad to supply fresh names
- ImpConfig.hs: options for running the system
- MyPrelude.hs
- Makefile
- readme.txt
- todo.txt


Primitives library:
- Primitives.imp (primitives interface - to be used for compilation)
- Primitives.c and Primitives.h (primitive library - to be used for linking)


Examples (Imp and ImpI versions):
- Bsearch.imp
- Bubblesort.imp
- Copyseq.imp
- Dotprod.imp
- Hanoi.imp
- Initarr.imp
- Mvm.imp
- Quicksort.imp
- Swaparr.imp
- TC.imp

