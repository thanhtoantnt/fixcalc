{- | Provides (1) Omega functions, (2) selector functions from annotated types and (3) functions that deal with Substitutions.
-}
module ImpFormula where
import Fresh(FS(..),fresh,takeFresh,addOmegaStr,getFlags,putStrFS,putStrFS_debug)
import ImpAST
import ImpConfig(isIndirectionIntArray,whatHull,Hull(..))
import InSolver(impSubset,impDifference,impSimplify,impGist,impHull,impConvexHull,impPairwiseCheck,impComplement)
import MyPrelude
------------------------------------------
import Data.List(nub,(\\),intersect,union)
import Data.Maybe(catMaybes)

equivalent:: Formula -> Formula -> FS Bool
-- ^Tests whether two formulae are equivalent.
equivalent r1 r2 =
  subset r1 r2 >>= \b1 ->
  subset r2 r1 >>= \b2 ->
  return (b1 && b2)

simplify:: Formula -> FS Formula
-- ^Simplifies a formula provided it does not have any AppRecPost. 
simplify f = 
  if countAppRecPost f==0 then 
    impSimplify (fqsv f,[],f)
  else 
    return f -- unsafe to simplify a formula with AppRecPost
    
subset:: Formula -> Formula -> FS Bool
-- ^Given f1 and f2, returns (f1 subset f2).
subset f1 f2 = 
  let v1 = fqsv f1 in let v2 = fqsv f2 in
  impSubset (v1,[],f1) (v2,[],f2)

complement:: Formula -> FS Formula
complement f1 =
  let v1 = fqsv f1 in
  impComplement (v1,[],f1)
  
difference:: Formula -> Formula -> FS Formula
-- ^Given f1 and f2, returns (f1 difference f2).
difference f1 f2 = 
  let v1 = fqsv f1 in let v2 = fqsv f2 in
  impDifference (v1,[],f1) (v2,[],f2)

hull:: Formula -> FS Formula
-- ^Applies a Hull or a ConvexHull operation depending on the flag 'whatHull'.
hull f = 
  getFlags >>= \flags ->
  (case whatHull flags of
    Hull -> impHull (fqsv f,[],f)
    ConvexHull -> impConvexHull (fqsv f,[],f)) >>= \res ->
  -- putStrFS ("\nInp:="++(show f)) >>
  -- putStrFS ("Hull:="++(show res)) >>
  return res
    
pairwiseCheck_disj:: [Formula] -> FS [Formula]
pairwiseCheck_disj f = 
  let fone = mkDisjunctsN f in
  pairwiseCheck fone >>=
  \ res -> return (getDisjunctsN res)

pairwiseCheck:: Formula -> FS Formula
pairwiseCheck f = impPairwiseCheck (fqsv f,[],f)

noChange:: [QSizeVar] -> Formula
-- ^Given some size-variables (x,y) it returns a formula: (x=x' && y=y'). Its arguments should be unprimed size-variables.
noChange qszVars = 
  let {f = \qs -> 
    case qs of
      (s,Primed) -> error $ "noChange: argument should not contain primed size variable"
      (s,Unprimed) -> EqK [Coef (s,Unprimed) 1,Coef (s,Primed) (-1)]}
  in 
  let fs = map f qszVars in
    fAnd fs

applyRecToPrimOnInvariant:: Formula -> FS Formula
applyRecToPrimOnInvariant inv =
  let qsv = fqsv inv in
  let recs = filter (\q -> case q of {(s,Recursive) -> True;_ -> False} ) qsv in
  let prims = map (\q -> case q of {(s,Recursive) -> (s,Primed);_ -> error "assertion failed in applyRecToPrimOnInvariant"}) recs in
  debugApply (zip recs prims) inv

-- phi may contain primed qsvs which must be unprimed
composition:: [QSizeVar] -> Formula -> Formula -> FS Formula
composition u delta phi = 
  let s = u in
  if (null s) then 
    return $ And[delta,phi]
  else
    takeFresh (length s) >>= \fshies -> 
    let r = map (\f -> (SizeVar f,Unprimed)) fshies in
    let rho = zip s r in
    let sp = map (\(sv,Unprimed) ->(sv,Primed)) s in 
    let rhop = zip sp r in
    debugApply rhop delta >>= \rhopDelta ->
    debugApply rho phi >>= \rhoPhi ->
    return (Exists r (And[rhopDelta,rhoPhi])) -- r is fresh. Exists can be used instead of fExists

-- phi should not contain primed qsvs 
ctxImplication:: [QSizeVar] -> Formula -> Formula -> FS Bool
ctxImplication u delta phi =
  let s = assertAllUnprimed (u `intersect` (fqsv phi)) in
  let sp = primeTheseQSizeVars s in
  let rhoPhi = apply (zip s sp) phi in
  let relDelta = (fqsv delta,[],delta) in
  let relPhi = (fqsv rhoPhi,[],rhoPhi) in
  addOmegaStr ("CTX:=" ++ showSet delta) >>
  addOmegaStr ("PHI:=" ++ show (map showTest3 (fqsv delta),rhoPhi)) >>
	impSubset relDelta relPhi >>= \result ->
  addOmegaStr ("CTX subset PHI; # Omega returns " ++ show result) >>
  return result

-- phi should not contain primed qsvs 
ctxSimplify::[QSizeVar] -> [QSizeVar] -> Formula -> Formula -> Formula -> FS Formula
ctxSimplify v u delta phi toGistWith = 
  let s = assertAllUnprimed (u `intersect` (fqsv phi)) in
  let sp = primeTheseQSizeVars s in
  let rhoPhi = apply (zip s sp) phi in
--    addOmegaStr ("PHI:=" ++ showSet rhoPhi) >>
  let satisf = (fOr [(fNot delta),rhoPhi]) in
  let f1 = fForall ((fqsv satisf) \\ v) satisf in
--    addOmegaStr ("CTX:=" ++ showSet delta) >>
--    addOmegaStr ("PRE:=" ++ showSet f1) >>
  let f2 = fExists ((fqsv toGistWith) \\ v) toGistWith in
--    addOmegaStr ("TO_GIST_WITH:=" ++ showSet f2) >>
  let rel1 = (fqsv f1,[],f1) in
  let rel2 = (fqsv f2,[],f2) in
    impGist rel1 rel2

gistCtxGivenInv:: Formula -> Formula -> FS Formula
gistCtxGivenInv delta typeInv = 
  let vars = nub ((fqsv delta) `union` (fqsv typeInv)) in
  let rel1 = (vars,[],delta) in
  let rel2 = (vars,[],typeInv) in
  impGist rel1 rel2

-- | This check should be done before ctxImplication and ctxSimplify(Rec).
-- Size variables from U (to be linked) are checked not be Primed! Should not happen - and may be disabled later.
assertAllUnprimed:: [QSizeVar] -> [QSizeVar]
assertAllUnprimed = map (\qs -> case qs of
  (sv,Primed) -> error $ "assertAllUnprimed: arguments should not be primed"
  (sv,Recursive) -> qs
  (sv,Unprimed) -> qs)

-------Selectors from Annotated Types-----
-- |Given a type ty, it generates unprimed versions of SizeVars found in ty. (same as unprimeThisTy)
fsvTy:: AnnoType -> FS [QSizeVar]
fsvTy ty = 
  sizeVarsFromAnnTy ty >>= \svs ->
  return $ map (\s -> (s,Unprimed)) svs

-- |Given a type ty, it generates both Primed and Unprimed versions of SizeVars found in ty.
fsvPUTy:: AnnoType -> FS [QSizeVar]
fsvPUTy ty = 
  sizeVarsFromAnnTy ty >>= \svs -> 
  return $ concatMap (\v -> (v,Unprimed):[(v,Primed)]) svs

-- |Given a type ty, it returns all size variables from this annotated type.
sizeVarsFromAnnTy:: AnnoType -> FS [SizeVar]
sizeVarsFromAnnTy (TopType{}) = return []
sizeVarsFromAnnTy ty@ArrayType{} =
  getFlags >>= \flags -> 
  mapM sizeVarsFromAnnTy (indxTypes ty) >>= \ps ->
  let p = concat ps in
  let e = if isIndirectionIntArray flags then
            case elemType ty of
              PrimInt{anno=Just a} -> (ArrSizeVar a Min):(ArrSizeVar a Max):[]
              PrimInt{anno=Nothing} -> error $ "sizeVarsFromAnnTy: indirection array (Int element type) is not annotated\n " ++ showImpp (elemType ty)
              _ -> []
          else [] in
    return (p++e)
sizeVarsFromAnnTy ty@PrimBool{} = case anno ty of 
  Nothing -> error $ "sizeVarsFromAnnTy: sized type is not annotated\n " ++ showImpp ty
  Just a -> return [(SizeVar a)]
sizeVarsFromAnnTy ty@PrimInt{} = case anno ty of 
  Nothing -> error $ "sizeVarsFromAnnTy: sized type is not annotated\n " ++ showImpp ty
  Just a -> return [(SizeVar a)]
sizeVarsFromAnnTy ty@PrimFloat{} = return []
sizeVarsFromAnnTy ty@PrimVoid{} = return []

-- |Changes all QSizeVars (given as argument) to Primed. The input list should not contain Recursive or Primed QSizeVars.
primeTheseQSizeVars:: [QSizeVar] -> [QSizeVar]
primeTheseQSizeVars = map (\q -> case q of 
  (sv,Unprimed) -> (sv,Primed)
  (sv,Primed) -> error $ "primeTheseQSizeVars: size variables from argument SHOULD NOT be primed: "++showImpp q
  )

-- |Changes all QSizeVars (given as argument) to Recursive. The input list should not contain Recursive or Primed QSizeVars.
recTheseQSizeVars:: [QSizeVar] -> [QSizeVar]
recTheseQSizeVars = map (\q -> case q of 
  (sv,Unprimed) -> (sv,Recursive)
  (sv,Primed) -> error $ "recTheseQSizeVars: size variables from argument SHOULD NOT be primed: "++showImpp q
  )

-------Rename - construct substitution----
type Subst = [(QSizeVar,QSizeVar)]

-- |Constructs a substitution from the two type arguments. It verifies that ty1 and ty2 have the same underlying type.
rename:: AnnoType -> AnnoType -> FS (Maybe Subst)
rename ty1 ty2 =
  case (ty1,ty2) of
    (ty1,TopType{}) -> return (Just [])
    (TopType{},ty2) -> return (Just [])
    (_,_) -> 
      fsvPUTy ty1 >>= \svs1 ->
      fsvPUTy ty2 >>= \svs2 ->
      if (sameBaseTy ty1 ty2 && (length svs1 == length svs2)) then
        return $ Just (zip svs1 svs2)
      else return Nothing
  
-- |Given a substitution [(x1,y1),(x2,y2)] it returns [(y1,x1),(y2,x2)].
inverseSubst:: Subst -> Subst
inverseSubst [] = []
inverseSubst ((x1,x2):xs) = (x2,x1):(inverseSubst xs)

-- |Given ty1 and ty2, it checks that the underlying types of ty1 and ty2 are the same.
sameBaseTy:: AnnoType -> AnnoType -> Bool
sameBaseTy ty1 ty2 = case (ty1,ty2) of
  (PrimBool{anno=a},PrimBool{anno=b}) -> True
  (PrimFloat{},PrimFloat{}) -> True
  (PrimInt{},PrimInt{}) -> True
  (PrimVoid{},PrimVoid{}) -> True
  --it does not check whether the indices of an array have the same type. All indices are assumed to be TyInt!
  (ArrayType{elemType=eTy1,indxTypes=iTys1},ArrayType{elemType=eTy2,indxTypes=iTys2}) -> 
    let sameElem= sameBaseTy eTy1 eTy2 in
    let sameNoDimensions = (length iTys1==length iTys2)
    in and $ sameElem:sameNoDimensions:[]
  (_,_) -> False

-------Apply Substitution to Formula------
-- |Checks whether the substitution has distinct elements. If not, prepareSubst is used before apply.
-- PrepareSubst is NECESSARY for recursive functions.
debugApply:: Subst -> Formula -> FS Formula
debugApply subst f = 
  let from = fst (unzip subst) in
  let to = snd (unzip subst) in
  let safeSubstFS = 
        if (length (nub from)) == (length from) then
            if null (from `intersect` to) then
              return subst 
            else prepareSubst subst []
        else error $ "debugApply: substitution does not have unique args: " ++ show subst
  in  safeSubstFS >>= \safeSubst ->
  return (apply safeSubst f)
  
-- |If the input subst is [c->d,d->e], the result of applying this subst to a formula will depend on the order of its pairs. (And this is incorrect).
-- This function implements Florin's solution: it transforms [c->d,d->e] to [c->f0,d->e,f0->d].
prepareSubst:: Subst -> Subst -> FS Subst
prepareSubst [] putToEnd = return putToEnd
prepareSubst ((s1,s2):ss) putToEnd =
  fresh >>= \fsh ->
  let fshQsv = (SizeVar fsh,Unprimed) in
  prepareSubst ss ((fshQsv,s2):putToEnd) >>= \preparedSS ->
  return ((s1,fshQsv):preparedSS)

apply:: Subst -> Formula -> Formula
apply [] f = f
apply (s:ss) f = apply ss (applyOne (fst s,snd s) f)
  where
  applyOne:: (QSizeVar,QSizeVar) -> Formula -> Formula
  applyOne (fromSV,toSV) f = case f of
    And formulae -> And (map (\f -> applyOne (fromSV,toSV) f) formulae)
    Or formulae -> Or (map (\f -> applyOne (fromSV,toSV) f) formulae)
    Not formula -> Not (applyOne (fromSV,toSV) formula)
    Exists otherSVs formula -> 
      if (elem fromSV otherSVs) 
        then f 
        else Exists otherSVs (applyOne (fromSV,toSV) formula)
    Forall otherSVs formula -> 
      if (elem fromSV otherSVs) 
        then f 
        else Forall otherSVs (applyOne (fromSV,toSV) formula)
    GEq updates -> GEq (map (\u -> applyOneToUpdate (fromSV,toSV) u) updates)
    EqK updates -> EqK (map (\u -> applyOneToUpdate (fromSV,toSV) u) updates)
    AppRecPost lit insouts -> 
        AppRecPost lit (map (\insout -> if insout==fromSV then toSV else insout) insouts)
    _ -> error ("applyOne: unexpected argument:" ++ showSet f)
    
  applyOneToUpdate:: (QSizeVar,QSizeVar) -> Update -> Update
  applyOneToUpdate (fromSV,toSV) up = case up of
    Const int -> up
    Coef otherSV int -> if otherSV==fromSV then Coef toSV int else up

isEqualF:: Formula -> Bool
isEqualF f = case f of
  EqK _ -> True
  _ -> False
  
getConjunctsN:: Formula -> [Formula]
-- requires: formula is conjunctive
getConjunctsN formula = case formula of
  And fs -> concatMap (\f -> getConjunctsN f) fs
  GEq us -> [formula]
  EqK us -> [formula]
  Exists qsvs f ->
    case getConjunctsN f of
      [] -> []
      _ -> [formula]
  Or fs -> 
    case fs of
      [x] -> getConjunctsN x
      _ -> []

getDisjunctsN:: Formula -> [Formula]
-- requires: formula is conjunctive
getDisjunctsN formula = case formula of
  Or fs -> fs
  _ -> [formula]

mkDisjunctsN:: [Formula] -> Formula
-- requires: formula is conjunctive
mkDisjunctsN fs = Or fs

