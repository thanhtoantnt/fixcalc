-- #ignore-exports
module Main(main) where
import Fresh(initialState,runFS,putStrFS,FS(..),getCPUTimeFS,addOmegaStr)
import ImpAST(Prog(..),MethDecl(..),methName,printProgImpi,printProgC,printProgImpt,getUpsisFromProg,fFalse,fTrue,showImpp)
import ImpConfig(defaultFlags,Flags(..),Postcondition(..),Prederivation(..),Heur(..),Hull(..))
import ImpFormula(subset)
import ImpOutInfer(outInferSccs,getNonTruePres)
import ImpParser(parse)
import ImpSugar(specialize,desugarInfer,desugarChecker)
import ImpSTypeChecker(sTypeCheck)
import ImpTypeChecker(typeCheckProg)
import ImpTypeInfer(typeInferSccs,methAdjacencies,getExternalMethods,setExternalMethods)
import MyPrelude
------------------------------------------
import Data.Graph(stronglyConnComp)
import Control.Monad(when)
import System.Environment(getArgs)
import System.CPUTime(getCPUTime)


main :: IO ()
main = 
  getArgs >>= \cmdLine ->
  processCmdLine cmdLine >>= \processedCmdLine ->
  case processedCmdLine of
    Nothing -> return ()
    Just (file,flags) ->
      readFile file >>= \meth ->
      let methIncl = if isIndirectionIntArray flags then "#include \"PrimitivesIndir.imp\"\n\n"++meth else meth in
      getCPUTime >>= \tStartCPU ->
      parse methIncl >>= \prog -> 
      putStrLn ("Parsing...done!") >>
      compile flags prog >>= \_ ->
      getCPUTime >>= \tEndCPU -> 
      putStrLn ("Total CPU time: " ++ showDiffTimes tEndCPU tStartCPU)

processCmdLine:: [String] -> IO (Maybe (String,Flags))
processCmdLine cmdLine = 
  case cmdLine of
    [] -> showHelpMessage
    ["--help"] -> showHelpMessage
    (source:args) -> 
--      putStrLn ("Default flags: "++show defaultFlags) >>
      allArgs defaultFlags args >>= \allA ->
      case allA of
        Nothing -> return Nothing
        Just flags ->
--          putStrLn ("CmdLine flags: "++show flags) >>
          return (Just (source,flags))

showHelpMessage = 
  putStrLn "Usage: imp file [options]" >>
  putStrLn "\nGeneral options:" >>
  putStrLn "  +infer -check\t\t Infer the input file and output the result in *.impt file." >>
  putStrLn "  +infer +check\t\t Infer the input file and type-check the result from inference." >>
  putStrLn "  -infer +check\t\t Type-check the input file annotated with pre/post (impt format)." >>
  putStrLn "  +indir\t\t Enable array contents analysis (experimental flag)." >>
  putStrLn "  -o:<file>\t\t Place the output in <file.impt>, <file.c> and <file.omega>." >>
  putStrLn "  -v:<level>\t\t Be verbose, where <level> is the verbosity level (0, 1 or 2)." >>
  putStrLn "   0\t\t\t Do not show any fixpoint messages." >>
  putStrLn "   1\t\t\t Show only loss-of-precision messages." >>
  putStrLn "   2\t\t\t Show loss-of-precision and hull/widening messages." >>
  putStrLn "\nPre/Postcondition derivation options:" >>
  putStrLn "  <pre><post>\t\t Use the derivations <pre><post>. From less to more precision, <pre> can be Post/Strong/Sel/Weak/Dual, <post> can be Weak/Strong." >>
  putStrLn "   PostStrong\t\t Necessary precondition (over-approximation) derived from the postcondition [ASIAN'06]." >>
  putStrLn "   StrongStrong\t\t Sufficient precondition (under-approximation). Other combinations: <Strong/Sel/Weak><Strong/Weak> [PEPM'08]." >>
  putStrLn "   DualStrong\t\t Sufficient precondition derived from dual analysis [under-submission]." >>
  putStrLn "\nDual analysis options:" >>
  putStrLn "  +individual\t\t Enable derivation of individual bug conditions." >>
  putStrLn "  +computeAll\t\t Compute all safety/bug conditions for all methods (as opposed to only external methods)." >>
  putStrLn "\nFixpoint options:" >>
  putStrLn "  -m:<bound>\t\t Use <bound>-disjunctive fixpoint, where <bound> is the maximum number of disjuncts." >>
  putStrLn "  -club:<lub>\t\t Use the conjunctive lub operator <lub>, where <lub> can be Hull or ConvexHull." >>
  putStrLn "  -dlub:<lub>\t\t Use the disjunctive lub operator <lub>, where <lub> can be Similarity, Hausdorff or Interactive." >>
  putStrLn "   Similarity\t\t Use the Planar affinity-based heuristic [ASIAN'06]." >>
  putStrLn "   Hausdorff\t\t Use the Hausdorff-based heuristic [Sriram et al-SAS'06]." >>
  putStrLn "   Interactive\t\t Allow user to specify interactively which disjuncts to combine, and revert to Similarity-based heuristic when unspecified." >>
  putStrLn "  +simplifyCAbst\t Enable simplification of constraint abstraction before fixpoint (experimental flag)." >>
  putStrLn "Default arguments: +infer -check -indir -o:a -v:0 DualStrong -individual +computeAll -m:5 -club:Hull -dlub:Similarity -simplifyCAbst" >>
  return Nothing

allArgs:: Flags -> [String] -> IO (Maybe Flags)
allArgs flags [] = return (Just flags)
allArgs flags (a:args) = 
  oneArg flags a >>= \one ->
  case one of
    Nothing -> return Nothing
    Just newFlags -> (allArgs newFlags args)

oneArg:: Flags -> String -> IO (Maybe Flags)
oneArg prevFs arg = case arg of
-- General options:
  "+infer" ->       return $ Just prevFs{noInference=False}
  "-infer" ->       return $ Just prevFs{noInference=True}
  "-check" ->       return $ Just prevFs{checkingAfterInference=False} 
  "+check" ->       return $ Just prevFs{checkingAfterInference=True}
  "-indir" ->       return $ Just prevFs{isIndirectionIntArray=False}
  "+indir" ->       return $ Just prevFs{isIndirectionIntArray=True}
  '-':'o':':':file -> return $ Just prevFs{outputFile=file}
  '-':'v':':':level -> 
                    case snd (fixFlags prevFs) of
                      SimInteractiveHeur -> return $ Just prevFs{showDebugMSG=2}
                      _ -> return $ Just prevFs{showDebugMSG=read level}
-- Pre/Postcondition derivation options:
  "PostStrong" ->   return $ Just prevFs{prederivation=PostPD,postcondition=StrongPost}
  "PostWeak" ->     return $ Just prevFs{prederivation=PostPD,postcondition=WeakPost}
  "StrongStrong" -> return $ Just prevFs{prederivation=StrongPD,postcondition=StrongPost}
  "StrongWeak" ->   return $ Just prevFs{prederivation=StrongPD,postcondition=WeakPost}
  "SelStrong" ->    return $ Just prevFs{prederivation=SelectivePD,postcondition=StrongPost}
  "SelWeak" ->      return $ Just prevFs{prederivation=SelectivePD,postcondition=WeakPost}
  "WeakStrong" ->   return $ Just prevFs{prederivation=WeakPD,postcondition=StrongPost}
  "WeakWeak" ->     return $ Just prevFs{prederivation=WeakPD,postcondition=WeakPost}
  "DualStrong" ->   return $ Just prevFs{prederivation=DualPD}
  "DualIStrong" ->  return $ Just prevFs{prederivation=DualPD,traceIndividualErrors=True}
-- Dual analysis options:
  "+individual" ->  return $ Just prevFs{traceIndividualErrors=True}
  "-individual" ->  return $ Just prevFs{traceIndividualErrors=False}
  "+computeAll" ->  return $ Just prevFs{computeAll=True}
  "-computeAll" ->  return $ Just prevFs{computeAll=False}
-- Fixpoint options:
  '-':'m':':':m ->  return $ Just prevFs{fixFlags=(read m,snd (fixFlags prevFs))}
  "-club:Hull" ->   return $ Just prevFs{whatHull=Hull}
  "-club:ConvexHull" -> return $ Just prevFs{whatHull=ConvexHull}
  "-dlub:Similarity" -> return $ Just prevFs{fixFlags=(fst (fixFlags prevFs),SimilarityHeur)}
  "-dlub:Hausdorff" -> return $ Just prevFs{fixFlags=(fst (fixFlags prevFs),HausdorffHeur)} 
  "-dlub:Interactive" -> return $ Just prevFs{fixFlags=(fst (fixFlags prevFs),SimInteractiveHeur),showDebugMSG=2} 
  "+simplifyCAbst" -> return $ Just prevFs{simplifyCAbst=True}
  "-simplifyCAbst" -> return $ Just prevFs{simplifyCAbst=False}
-- Options from the old system (2004-2006):
  "-sep" ->         return $ Just prevFs{separateFstFromRec=False}
  "+sep" ->         return $ Just prevFs{separateFstFromRec=True}
  "-selHull" ->     return $ Just prevFs{useSelectiveHull=False}
  "+selHull" ->     return $ Just prevFs{useSelectiveHull=True}
  "-widenEarly" ->  return $ Just prevFs{widenEarly=False}
  "+widenEarly" ->  return $ Just prevFs{widenEarly=True}

  _ -> 
    putStrLn ("imp: unrecognised flag: " ++ arg) >>
    showHelpMessage

compile:: Flags -> Prog -> IO Prog
compile flags prog = 
  if noInference flags then 
      runFS (initialState flags) (
          	desugarChecker prog >>= \dsgProg ->
            typeCheckProg dsgProg
      )
  else 
  runFS (initialState flags) (
        sTypeCheck prog >>= \noWhileProg -> 
        putStrFS "Simple type-checking...done!" >>
        setExternalMethods noWhileProg >>= \withExtProg ->
        desugarInfer withExtProg >>= \dsgProg@(Prog _ prims meths) -> 
        -- Print C code without any bound-checks:
        --  printProgCAll dsgProg >>
        let sccs = stronglyConnComp (methAdjacencies meths) in
        addOmegaStr "# Starting inference..." >>
        (if prederivation flags == DualPD then
          getCPUTimeFS >>= \time1 -> outInferSccs dsgProg sccs >>= \infProg -> getCPUTimeFS >>= \time2 ->
          putStrFS ("Inference...done in " ++ showDiffTimes time2 time1) >> 
          addOmegaStr "# Inference is finished...\n\n" >>
          getExternalMethods infProg >>= \externalMeths ->
          mapM (printDualResult flags (not (singleton externalMeths))) externalMeths >> return infProg
        else 
          getCPUTimeFS >>= \time1 -> typeInferSccs dsgProg sccs >>= \infProg -> getCPUTimeFS >>= \time2 ->
          putStrFS ("Inference...done in " ++ showDiffTimes time2 time1) >> 
          addOmegaStr "# Inference is finished...\n\n" >>
          -- Print result of inference before specialization:
          -- printProgImpi infProg >>
          getExternalMethods infProg >>= \externalMeths ->
          let upsis = getUpsisFromProg infProg in
          getNonTruePres externalMeths >>= \nontrue ->
          if (snd upsis == 0 && null nontrue) then 
            putStrFS ("SAFETY: All checks in the program were proven!") >>
            return infProg
          else 
            if (snd upsis /= 0) then 
              let str = if (snd upsis == 1) then " runtime test needed: " else " runtime tests needed: " in
              putStrFS ("POSSIBLE bug(s):\n" ++ show (snd upsis) ++ str ++ (fst upsis)) >>
              when (length nontrue /=0) (putStrFS ("Preconditions not valid: " ++ show nontrue)) >>
              getCPUTimeFS >>= \time1 -> specialize infProg >>= \specializedProg -> getCPUTimeFS >>= \time2 -> 
              putStrFS ("Specialization...done in " ++ showDiffTimes time2 time1) >> return specializedProg
            else -- some preconditions are not true
              putStrFS ("POSSIBLE bug(s):\n" ++ "Preconditions not valid: " ++ show nontrue) >> return infProg
          ) >>= \afterInferenceProg -> 
          printProgC afterInferenceProg >>
          printProgImpt afterInferenceProg >>
          if (checkingAfterInference flags) then 
          	desugarChecker afterInferenceProg >>= \dsgProg ->
            typeCheckProg dsgProg
          else 
            return afterInferenceProg
  )

printDualResult:: Flags -> Bool -> MethDecl -> FS ()
-- ^Uses methNEVER, methMUSTs and methMAY to print results from dual analysis.
printDualResult flags printName m = 
  putStrFS ("============") >>
  when (printName) (putStrFS ("Inference result for the " ++ methName m ++ " method:")) >>
  ( -- print NEVER_BUG
    subset (methNEVER m) fFalse >>= \isUnsatisfiable -> 
    subset fTrue (methNEVER m) >>= \isValid -> 
    if isValid then
      putStrFS ("SAFETY: All checks in the program were proven!")
    else if isUnsatisfiable then
      return ()
    else -- methNEVER is neither True nor False
      putStrFS ("SAFETY: All checks in the program were proven for condition NEVER_BUG = " ++ show (methNEVER m))
  ) >> (-- print MUST_BUGS
    if traceIndividualErrors flags then 
      if null (methMUSTs m) then -- methMUSTs are False
        return ()
      else
        mapM (\(lbl,f) -> putStrFS ("BUG found for condition MUST_BUG_" ++ showImpp lbl ++ " = " ++ show f)) (methMUSTs m) >> return ()
    else
      if null (methMUSTs m) then -- methMUST is False
        return ()
      else 
        subset fTrue (snd (head (methMUSTs m))) >>= \isValid ->
        subset (snd (head (methMUSTs m))) fFalse >>= \isUnsatisfiable ->
        if isValid then 
          putStrFS ("BUG(S) found.")
        else if isUnsatisfiable then
          return ()
        else -- methMUST is neither True nor False
          putStrFS ("BUG(S) found for condition MUST_BUG = " ++ show (snd (head (methMUSTs m))))
  ) >> ( -- print MAY_BUG
    let (lbl,may) = methMAY m in
    subset may fFalse >>= \isUnsatisfiable ->
    if isUnsatisfiable then
      return ()
    else
      if traceIndividualErrors flags then
        putStrFS ("POSSIBLE bug(s) for condition MAY_BUG_" ++ showImpp lbl ++ " = " ++ show may)
      else
        putStrFS ("POSSIBLE bug(s) for condition MAY_BUG = " ++ show may))
  