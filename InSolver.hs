{- | 
  Provides an interface to the Omega functions. 
  Does some conversions (Imp->Omega) and (Omega->Imp).
-}
module InSolver(impSubset,impSimplify,impGist,impHull,impConvexHull,impUnion,impDifference,impComplement,impCompose,impPairwiseCheck) where
import qualified Omega as Omega
import qualified PFOmega as Omega
import qualified Omega_types as Omega
import qualified Omega_parser as Omega
import qualified Omega_stub as Omega_stub
-----------------------------
import ImpAST(Relation(..),Formula(..),Update(..),QSizeVar(..),Test3(..),showTest3, SizeVar(..),PorU(..),MorM(..),
    fAnd,fOr,fExists,fForall,fFalse,stringToQsv)
import Fresh(FS(..),fresh,addOmegaStr,putStrFS,putStrFS_debug)
import MyPrelude
-----------------------------
import Foreign(nullPtr,unsafePerformIO)
import Data.List(union,intersect)

impSimplify:: Relation -> FS Formula
impSimplify (qsv1,[],f1) =
  let vf1 = map showTest3 qsv1 in
  let impF1 = canonicalF f1 in
  -- addOmegaStr (show (vf1,impF1)) >>
  -- putStrFS (show (vf1,impF1)) >>
  let rf1 = Omega.replace_vars_in_rformula vf1 (Omega.Formula (impToOmF impF1)) in
  let back_rf = unsafePerformIO (Omega.simplify (vf1,[],rf1)) in
  replace_vars_from_rformula vf1 back_rf >>= \repl_back_rf ->
  let res = removeUnions (repl_back_rf) in
  -- addOmegaStr ("Simpl:=" ++ show (vf1,res)) >> 
  -- putStrFS ("Simpl:=" ++ show (vf1,res)) >> 
  return res

impSubset:: Relation -> Relation -> FS Bool
impSubset (qsv1,[],f1) (qsv2,[],f2) = 
  let vf1Uf2 = map showTest3 (qsv1 `union` qsv2) in
  let impF1 = canonicalF f1 in
  let impF2 = canonicalF f2 in
--    addOmegaStr ("# Subset? ") >> 
--    addOmegaStr (show (vf1Uf2,impF1)) >> addOmegaStr (show (vf1Uf2,impF2)) >>
  let rf1 = Omega.replace_vars_in_rformula vf1Uf2 (Omega.Formula (impToOmF impF1)) in
  let rf2 = Omega.replace_vars_in_rformula vf1Uf2 (Omega.Formula (impToOmF impF2)) in
  let back_bool = unsafePerformIO (Omega.subset (vf1Uf2,[],rf1) (vf1Uf2,[],rf2)) in
  let answer = if back_bool then "YES" else "NO" in
--    addOmegaStr ("# Subset? " ++ answer) >> 
  return back_bool

impGist:: Relation -> Relation -> FS Formula
impGist (qsv1,[],f1) (qsv2,[],f2) =
  impSimplify (qsv1,[],f1) >>= \f1 ->
  impSimplify (qsv2,[],f2) >>= \f2 ->
  let vf1Uf2 = map (\s-> show (Test3 s)) (qsv1 `union` qsv2) in
  let impF1 = canonicalF f1 in
  let impF2 = canonicalF f2 in
--    addOmegaStr (show (vf1Uf2,impF1)) >> addOmegaStr (show (vf1Uf2,impF2)) >> 
  let rf1 = Omega.replace_vars_in_rformula vf1Uf2 (Omega.Formula (impToOmF impF1)) in
  let rf2 = Omega.replace_vars_in_rformula vf1Uf2 (Omega.Formula (impToOmF impF2)) in
  let back_rf = unsafePerformIO (Omega.gist (vf1Uf2,[],rf1) (vf1Uf2,[],rf2)) in
  replace_vars_from_rformula vf1Uf2 back_rf >>= \repl_back_rf ->
  let res = removeUnions repl_back_rf in
--    addOmegaStr ("Gisted:=" ++ show (vf1Uf2,res)) >> 
  return res

impHull:: Relation -> FS Formula
impHull (qsv1,[],f1) =
  let vf1 = map showTest3 qsv1 in
  let impF1 = canonicalF f1 in
--    addOmegaStr ("BefHull:=" ++ show (vf1,impF1)) >>
  let rf1 = Omega.replace_vars_in_rformula vf1 (Omega.Formula (impToOmF impF1)) in
  let back_rf = unsafePerformIO (Omega.hull0 (vf1,[],rf1)) in
  replace_vars_from_rformula vf1 back_rf >>= \repl_back_rf ->
  let res = removeUnions repl_back_rf in
--    addOmegaStr ("AftHull:=" ++ show (vf1,res)) >> 
  return res

impConvexHull:: Relation -> FS Formula
impConvexHull (qsv1,[],f1) =
  let vf1 = map showTest3 qsv1 in
  let impF1 = canonicalF f1 in
--    addOmegaStr ("BefConvexHull:=" ++ show (vf1,impF1)) >>
  let rf1 = Omega.replace_vars_in_rformula vf1 (Omega.Formula (impToOmF impF1)) in
  let back_rf = unsafePerformIO (Omega.convex_hull (vf1,[],rf1)) in
  replace_vars_from_rformula vf1 back_rf >>= \repl_back_rf ->
  let res = removeUnions repl_back_rf in
--    addOmegaStr ("AftConvexHull:=" ++ show (vf1,res)) >> 
  return res

impUnion:: Relation -> Relation -> FS Formula
impUnion (qsv1,qsv1',f1) (qsv2,qsv2',f2) = 
  let vf1 = map showTest3 qsv1 in
  let vf2 = map showTest3 qsv2 in
  let vf1' = map showTest3 qsv1' in
  let vf2' = map showTest3 qsv2' in
  let impF1 = canonicalF f1 in
  let impF2 = canonicalF f2 in
--    addOmegaStr (show (vf1,vf1',impF1)) >> 
--    addOmegaStr (show (vf2,vf2',impF2)) >> 
  let rf1 = Omega.replace_vars_in_rformula (vf1 `union` vf1') (Omega.Formula (impToOmF impF1)) in
  let rf2 = Omega.replace_vars_in_rformula (vf2 `union` vf2') (Omega.Formula (impToOmF impF2)) in
  let back_rf = unsafePerformIO (Omega.union_relation (vf1,vf1',rf1) (vf2,vf2',rf2)) in
  let res_vars = vf1 `union` vf1' in
  replace_vars_from_rformula res_vars back_rf >>= \repl_back_rf ->
  let res = removeUnions repl_back_rf in
    addOmegaStr ("Unioned:=" ++ show (vf1,vf1',res)) >> 
  return res
  
impDifference:: Relation -> Relation -> FS Formula
impDifference (qsv1,qsv1',f1) (qsv2,qsv2',f2) = 
  let vf1 = map showTest3 qsv1 in
  let vf2 = map showTest3 qsv2 in
  let vf1' = map showTest3 qsv1' in
  let vf2' = map showTest3 qsv2' in
  let impF1 = canonicalF f1 in
  let impF2 = canonicalF f2 in
--    addOmegaStr (show (vf1,vf1',impF1)) >> 
--    addOmegaStr (show (vf2,vf2',impF2)) >> 
  let rf1 = Omega.replace_vars_in_rformula (vf1 `union` vf1') (Omega.Formula (impToOmF impF1)) in
  let rf2 = Omega.replace_vars_in_rformula (vf2 `union` vf2') (Omega.Formula (impToOmF impF2)) in
  let back_rf = unsafePerformIO (Omega.difference (vf1,vf1',rf1) (vf2,vf2',rf2)) in
  let res_vars = vf1 `union` vf1' in
  replace_vars_from_rformula res_vars back_rf >>= \repl_back_rf ->
  let res = removeUnions repl_back_rf in
    addOmegaStr ("Differenced:=" ++ show (vf1,vf1',res)) >> 
  return res

impCompose:: Relation -> Relation -> FS Formula
impCompose (qsv1,qsv1',f1) (qsv2,qsv2',f2) = 
  let vf1 = map showTest3 qsv1 in
  let vf2 = map showTest3 qsv2 in
  let vf1' = map showTest3 qsv1' in
  let vf2' = map showTest3 qsv2' in
  let impF1 = canonicalF f1 in
  let impF2 = canonicalF f2 in
--    addOmegaStr (show (vf1,vf1',impF1)) >> 
--    addOmegaStr (show (vf2,vf2',impF2)) >> 
  let rf1 = Omega.replace_vars_in_rformula (vf1 `union` vf1') (Omega.Formula (impToOmF impF1)) in
  let rf2 = Omega.replace_vars_in_rformula (vf2 `union` vf2') (Omega.Formula (impToOmF impF2)) in
  let back_rf = unsafePerformIO (Omega.composition (vf1,vf1',rf1) (vf2,vf2',rf2)) in
  let res_vars = vf2 `union` vf1' in
  replace_vars_from_rformula res_vars back_rf >>= \repl_back_rf ->
  let res = removeUnions repl_back_rf in
--    addOmegaStr ("Composed:=" ++ show (vf2,vf1',res)) >> 
  return res

impPairwiseCheck:: Relation -> FS Formula
impPairwiseCheck (qsv1,qsv1',f1) =
  let vf1 = map showTest3 qsv1 in
  let vf1' = map showTest3 qsv1' in
  let impF1 = canonicalF f1 in
  --    addOmegaStr (show (vf1,vf1',impF1)) >>
  let rf1 = Omega.replace_vars_in_rformula (vf1 `union` vf1') (Omega.Formula (impToOmF impF1)) in
  let back_rf = unsafePerformIO (Omega.pairwiseCheck (vf1,vf1',rf1)) in
  replace_vars_from_rformula vf1 back_rf >>= \repl_back_rf ->
  let res = removeUnions (repl_back_rf) in
--    addOmegaStr ("PairwiseChk:=" ++ show (vf1,vf1',res)) >> 
  return res

impComplement:: Relation -> FS Formula
impComplement (qsv1,qsv1',f1) =
  let vf1 = map showTest3 qsv1 in
  let vf1' = map showTest3 qsv1' in
  let impF1 = canonicalF f1 in
  let rf1 = Omega.replace_vars_in_rformula (vf1 `union` vf1') (Omega.Formula (impToOmF impF1)) in
  let back_rf = unsafePerformIO (Omega.complement (vf1,vf1',rf1)) in
  replace_vars_from_rformula vf1 back_rf >>= \repl_back_rf ->
  let res = removeUnions (repl_back_rf) in
  return res

-------Imp -> Omega----------------
impToOmF:: Formula -> Omega.Formula
impToOmF (And fs) = Omega.And (map impToOmF fs)
impToOmF (Or fs) = Omega.Or (map impToOmF fs)
impToOmF (Not f) = Omega.Not (impToOmF f)
impToOmF (EqK ups) = Omega.Eq (map impToOmU ups)
impToOmF (GEq ups) = Omega.Geq (map impToOmU ups)
impToOmF (Exists qsvs f) = Omega.And [Omega.exists_vars_in_formula (map showTest3 qsvs) (impToOmF f)]
impToOmF (Forall qsvs f) = Omega.And [Omega.forall_vars_in_formula (map showTest3 qsvs) (impToOmF f)]
impToOmF f@(AppRecPost name _) = error $ "Formula to be passed to Omega contains an AppRecPost:\n"++show f

impToOmU:: Update -> Omega.Update
impToOmU (Coef qsv i) = Omega.Coef (showTest3 qsv,nullPtr) i
impToOmU (Const i) = Omega.Const i

canonicalF:: Formula -> Formula
canonicalF formula = case formula of
  (EqK ups) -> And [formula]
  (GEq ups) -> And [formula]
  (And fs) -> formula
  (Or fs) -> formula
  (Not f) -> And [Not f]
  (Exists qs f) -> And [formula]
  (Forall qs f) -> And [formula]
  
-------Omega -> Imp----------------
removeUnions:: Formula -> Formula
removeUnions (Union fs) = Or fs
removeUnions f = f

-- Omega answers containing existential quantification need freshening
replace_vars_from_rformula:: [String] -> Omega.RFormula -> FS Formula
replace_vars_from_rformula [] (Omega.RFormula _) = error "too few string variables..impossible"
replace_vars_from_rformula [] (Omega.Formula f) = replace_vars_from_formula f
replace_vars_from_rformula vs (Omega.Union rfs) = 
  mapM (replace_vars_from_rformula vs) rfs >>= \replRfs ->
  return (Union replRfs)
replace_vars_from_rformula (v_name:v_names) rf1 = case rf1 of
  Omega.RFormula(rf2) -> 
    let v = (v_name,nullPtr) in
      replace_vars_from_rformula v_names (rf2 v)
  Omega.Formula f -> error "too many string variables..impossible"

replace_vars_from_formula:: Omega.Formula -> FS Formula
replace_vars_from_formula f = case f of
  Omega.And fs -> 
    mapM replace_vars_from_formula fs >>= \replFs ->
    return (fAnd replFs)
  Omega.Or fs -> 
    mapM replace_vars_from_formula fs >>= \replFs ->
    return (fOr replFs)
  Omega.Not f -> 
    replace_vars_from_formula f >>= \replF ->
    return (Not replF)
  Omega.Exists f ->
    fresh >>= \fsh ->
    let v = (fsh,nullPtr) in
    replace_vars_from_formula (f v) >>= \replF ->
    return $ fExists [(SizeVar fsh,Unprimed)] replF
  Omega.Forall f -> --any forall coming back from Omega?
    error $ "existential quantifier in Omega answer\n "
  Omega.Geq us -> return $ GEq (map replace_vars_from_update us)
  Omega.Eq us -> return $ EqK (map replace_vars_from_update us)
-- Unknown is converted to False 
-- conservative: in the only example where UNKNOWN occurs, the formula is a precondition 
  Omega.Unknown -> return $ (tra ("Unknown converted to "++show fFalse++"\n") fFalse)

replace_vars_from_update:: Omega.Update -> Update
replace_vars_from_update u = case u of
  Omega.Const i -> Const i
  Omega.Coef (v_name,v_ptr) i -> Coef (stringToQsv v_name) i

-------Test Formulae---------------
exCtxL1 = Exists [qsvI] (And [
  EqK [Coef qsvJ 1,Coef qsvI (-1),Const 1],
  GEq [Coef qsvI 1,Const (-1)],
  GEq [Coef qsvN 1,Coef qsvI (-1)],
  GEq [Coef qsvL 1,Const (-1)]])

preL1 = Or [
  GEq [Coef qsvL 1,Coef qsvJ (-1),Const (-2)],
  And [GEq [Coef qsvJ 1,Coef qsvN (-1)],GEq [Coef qsvJ 1,Coef qsvL (-1),Const 1]],
  And [GEq [Coef qsvL (-1)],GEq [Coef qsvJ 1,Coef qsvL (-1),Const 1],GEq [Coef qsvN 1,Coef qsvJ (-1),Const (-1)]]]

vexCtxL1 = ["i","j","n","l"]
vpreL1 = ["i","j","n","l"]

qsvI = (SizeVar "i",Unprimed)
qsvJ = (SizeVar "j",Unprimed)
qsvN = (SizeVar "n",Unprimed)
qsvL = (SizeVar "l",Unprimed)

testf1 = And [EqK [Coef qsvX 1],GEq [Coef qsvY 1,Const 10]]
testvf1 = ["x","y"]
testf2 = And [EqK [Coef qsvA 1],GEq [Coef qsvB 1,Const 1]]
testvf2 = ["a","b"]

qsvX = (SizeVar "x",Unprimed)
qsvY = (SizeVar "y",Unprimed)
qsvA = (SizeVar "a",Unprimed)
qsvB = (SizeVar "b",Unprimed)
