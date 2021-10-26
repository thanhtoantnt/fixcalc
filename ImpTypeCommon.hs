-- |Implements a type-environment and functions that deal with types (common to both checking and inference). 

module ImpTypeCommon(
  -- *Type environment
  TypeEnv, extendTypeEnv, lookupVar,
  initialTransFromTyEnv, invFromTyEnv,
  impFromTyEnv, nonimpFromTyEnv, qsvFromTyEnv,

  -- *Various functions to collect size variables from types
  setsForParamPassing, 
  equate, equateNonImpToPrim,
  initialTransFromTy, initArrFormula, impFromTy, 
  freshTy, genLabelArr
) where
import ImpAST
import ImpConfig(isIndirectionIntArray)
import Fresh(FS,fresh,takeFresh,getFlags,putStrFS)
import ImpFormula(fsvTy,noChange,primeTheseQSizeVars,sameBaseTy,sizeVarsFromAnnTy)
import MyPrelude(tr,concatMapM,snd3,fst3)
------------------------------------------
import Data.List(nub,union)
import Data.Maybe(fromJust)

-------TypeEnvironment-------------
type TypeEnv = [(Lit,AnnoType)]

extendTypeEnv :: TypeEnv -> (Lit,AnnoType) -> TypeEnv
extendTypeEnv gamma (var,ty) = 
  (var,ty):gamma

lookupVar :: Lit -> TypeEnv -> Maybe AnnoType
lookupVar lit [] = Nothing
lookupVar lit env@((v,ty):rest) | (lit == v) = Just ty
  | otherwise = lookupVar lit rest

{- | Given a function, it computes five sets of QSizeVar:

  1. inputs: set of boundary size variables. To be used in type judgement (V)

  2. outputs: primed qsvByRef + return

  3. res: set of size variables in return type

  4. qsvByRef: imperative size variables that are passed by reference. x \subset Imp(v)
 
  5. qsvByVal: imperative size variables that are passed by value. y \subset Imp(v) and x+y = Imp(v)
-}
setsForParamPassing:: Callable -> FS ([QSizeVar],[QSizeVar],[QSizeVar],[QSizeVar],[QSizeVar])
setsForParamPassing (Meth m) = 
  let ((passbyM,t,fname):args) = methParams m in
  concatMapM (\(_,tyi,argi) -> fsvTy tyi) args >>= \inputs ->
  fsvTy t >>= \res ->
  impFromTy t >>= \impRes ->
  case passbyM of
    PassByRef ->
      return [] >>= \qsvByVal ->
      concatMapM (\(_,tyi,namei) -> impFromTy tyi) args >>= \qsvByRef ->
      let outputs = primeTheseQSizeVars qsvByRef ++ impRes in
--      putStrFS ("Sets-FnByRef: " ++ show inputs ++ "\t" ++ show res ++ "\t" ++ show qsvByRef ++ "\t" ++ show qsvByVal ++ "\n") >>
      return (inputs,outputs,res,qsvByRef,qsvByVal)
    PassByVal ->
      concatMapM (\(passbyi,tyi,namei) -> case tyi of {ArrayType{} -> return [];_ -> if (passbyi==PassByVal) then impFromTy tyi else return []}) args >>= \qsvByVal ->
      concatMapM (\(passbyi,tyi,namei) -> case tyi of {ArrayType{} -> minmaxFromTy tyi;_ -> if (passbyi==PassByRef) then impFromTy tyi else return []}) args >>= \qsvByRef ->
      let outputs = primeTheseQSizeVars qsvByRef ++ impRes in
--      putStrFS ("Sets-FnByVal: " ++ show inputs ++ "\t" ++ show res ++ "\t" ++ show qsvByRef ++ "\t" ++ show qsvByVal ++ "\n") >>
      return (inputs,outputs,res,qsvByRef,qsvByVal)

setsForParamPassing (Prim p) = 
  let ((_,t,fname):args) = primParams p in
  concatMapM (\(_,tyi,argi) -> fsvTy tyi) args >>= \inputs ->
  concatMapM (\(_,tyi,argi) -> impFromTy tyi) args >>= \impInputs ->
  fsvTy t >>= \res ->
  impFromTy t >>= \impRes ->
  let outputs = primeTheseQSizeVars impInputs `union` impRes in
  concatMapM (\(_,tyi,namei) -> case tyi of {ArrayType{} -> return [];_ -> impFromTy tyi}) args >>= \qsvByVal ->
  concatMapM (\(_,tyi,namei) -> minmaxFromTy tyi) args >>= \qsvByRef ->
    return (inputs,outputs,res,qsvByRef,qsvByVal)

-- result: list containing min-max size variables (passed by reference)
minmaxFromTy:: AnnoType -> FS [QSizeVar]
minmaxFromTy ty | isPrimitiveType ty = return []
minmaxFromTy ArrayType{elemType=eTy,indxTypes=iTys} =
  getFlags >>= \flags ->
  if (isIndirectionIntArray flags && (sameBaseTy eTy (PrimInt{anno=Nothing}))) then
    return [(ArrSizeVar (fromJust (anno eTy)) Min,Unprimed),(ArrSizeVar (fromJust (anno eTy)) Max,Unprimed)]
  else return []

-- |Given an annotated type, it replaces all annotations by fresh ones.
freshTy:: AnnoType -> FS AnnoType
freshTy ty = case ty of
  PrimBool{} -> fresh >>= \fsh -> return $ PrimBool{anno=Just fsh}
  PrimFloat{} -> return $ PrimFloat{anno=Nothing}
  PrimInt{} -> fresh >>= \fsh -> return $ PrimInt{anno=Just fsh}
  PrimVoid{} -> return $ PrimVoid{anno=Nothing}
  ArrayType{elemType=eTy,indxTypes=iTys} ->
    mapM freshTy iTys >>= \fshITys ->
    freshTy eTy >>= \fshETy ->
    return $ ArrayType{elemType=fshETy,indxTypes=fshITys}

-- |Given the typeEnv {x::Int, a::[Float]Int, b::Bool}, it returns the formula: (x'=x && a>0 && 0<=b<=1).
initialTransFromTyEnv:: TypeEnv -> FS Formula
initialTransFromTyEnv tenv = 
  mapM (\(v,ty) -> initialTransFromTy ty) tenv >>= \fs ->
  let fsNoTrue = filter (\f -> case f of {EqK [Const 0] -> False;_ -> True}) fs in
  return (fAnd fsNoTrue)

initialTransFromTy:: AnnoType -> FS Formula
initialTransFromTy ty = 
  isIndirectionArrTy ty >>= \isIndir ->
  impFromTy ty >>= \tys ->
  let noX = noChange tys in
  let initp = case ty of
        PrimBool{anno=Just a} ->
          let qb = (SizeVar a,Unprimed) in
            fOr [EqK [Coef qb (-1),Const 1],EqK [Coef qb 1]]
-- the conjunct on the next line should be equivalent with (and more efficient than) the disjunct on the previous line
--            fAnd [GEq [Coef qb (-1),Const 1],GEq [Coef qb 1]] 
        ArrayType{elemType=eTy,indxTypes=iTys} ->
          let sGT0 s = fGT[Coef (SizeVar s,Unprimed) 1] in
          let sGT0s = map (\PrimInt{anno=Just s} -> sGT0 s) iTys in
          if isIndir then
            case anno eTy of
              Just elemAnno ->
                let min = ((ArrSizeVar elemAnno Min),Unprimed) in
                let max = ((ArrSizeVar elemAnno Max),Primed) in
                let minLTEmax = GEq [Coef max 1,Coef min (-1)] in
                  fAnd (minLTEmax:sGT0s) 
              Nothing -> error $ "indirection array without annotation ??" ++ showImpp ty
          else 
            fAnd sGT0s
        _ -> fTrue 
  in return $ (fAnd [noX,initp])

-- | Given the typeEnv {x::Int, a::[Float]Int, b::Bool}, it returns the formula (a>0 && 0<=b<=1).
invFromTyEnv:: TypeEnv -> FS Formula
invFromTyEnv tenv = 
  mapM (\(v,ty) -> invFromTy ty) tenv >>= \fs ->
  let fsNoTrue = filter (\f -> case f of {EqK [Const 0] -> False;_ -> True}) fs in
  return (fAnd fsNoTrue)

invFromTy:: AnnoType -> FS Formula
invFromTy ty = 
  isIndirectionArrTy ty >>= \isIndir ->
  let initp = case ty of
        PrimBool{anno=Just a} ->
          let qb = (SizeVar a,Unprimed) in
            fOr [EqK [Coef qb (-1),Const 1],EqK [Coef qb 1]]
-- the conjunct on the next line should be equivalent with (and more efficient than) the disjunct on the previous line
--            fAnd [GEq [Coef qb (-1),Const 1],GEq [Coef qb 1]] 
        ArrayType{elemType=eTy,indxTypes=iTys} ->
          let sGT0 s = fGT[Coef (SizeVar s,Unprimed) 1] in
          let sGT0s = map (\PrimInt{anno=Just s} -> sGT0 s) iTys in
          if isIndir then
            case anno eTy of
              Just elemAnno ->
                let min = ((ArrSizeVar elemAnno Min),Unprimed) in
                let max = ((ArrSizeVar elemAnno Max),Primed) in
                let minLTEmax = GEq [Coef max 1,Coef min (-1)] in
                  fAnd (minLTEmax:sGT0s) 
              Nothing -> error $ "indirection array without annotation ??" ++ showImpp ty
          else 
            fAnd sGT0s
        _ -> fTrue 
  in return initp

-------Equate types -> Formula------------
{- |
  1. Given ty1 and ty2, it verifies that they have the same underlying type. 
  The error could be more informative if sameBaseTy is moved to the place from where equate is called.

  2. Size variables from ty1 and ty2 are primed (or unprimed) depending on the arguments pORu1 and pORu2.
  
  3. It generates equalities between corresponding size variables from ty1 and ty2.
-}
equate:: (AnnoType,AnnoType) -> (PorU,PorU) -> FS (Maybe Formula)
equate (ty1,ty2) (pORu1,pORu2) = 
  if (sameBaseTy ty1 ty2) then
    (if (pORu1==Primed) then primeThisTy ty1 else unprimeThisTy ty1) >>= \pORuTy1 ->
    (if (pORu2==Primed) then primeThisTy ty2 else unprimeThisTy ty2) >>= \pORuTy2 ->
    if (length pORuTy1 /= length pORuTy2) then return Nothing
    else
      let zippedPU = zip pORuTy1 pORuTy2 in
      let eqs = map (\pair -> EqK [Coef (fst pair) 1,Coef (snd pair) (-1)]) zippedPU in
      if (null eqs) then 
        return (Just fTrue)
      else return $ Just (fAnd eqs)
  else return Nothing

-- |Given a list of types, it generates equalities of  the form (f0=f0' && s0=s0') for the non-imp size variables.
equateNonImpToPrim:: [AnnoType] -> FS Formula
equateNonImpToPrim tys = 
  mapM nonImpFromTy tys >>= \nitys ->
  let qsvNonImps = concat nitys in
  let fs = map (\(ty,Unprimed) -> EqK [Coef (ty,Unprimed) 1,Coef (ty,Primed) (-1)]) qsvNonImps in
    return (fAnd fs)

--returns a primed version of all size variables from primTy
primeThisTy:: AnnoType -> FS [QSizeVar]
primeThisTy ty = 
  impFromTy ty >>= \ity ->
  nonImpFromTy ty >>= \nity ->
  return (primeTheseQSizeVars ity ++ nity)
--returns an unprimed version of all size variables from primTy
unprimeThisTy:: AnnoType -> FS [QSizeVar]
unprimeThisTy ty = 
  sizeVarsFromAnnTy ty >>= \svs -> 
  return $ map (\s -> (s,Unprimed)) svs

-- |qsvFromTyEnv tenv == (impFromTyEnv tenv ++ nonimpFromTyEnv tenv).
qsvFromTyEnv:: TypeEnv -> FS [QSizeVar]
qsvFromTyEnv tenv = 
  impFromTyEnv tenv >>= \imps -> nonimpFromTyEnv tenv >>= \nonimps ->
  return (imps++nonimps)

-- |It returns a list containing imperative size variables (all but sizes of arrays).
impFromTyEnv:: TypeEnv -> FS [QSizeVar]
impFromTyEnv tenv = 
  mapM (\(v,ty) -> impFromTy ty) tenv >>= \imps ->
  return (concat imps)

-- |It returns a list containing non-imperative size variables (sizes of arrays).
nonimpFromTyEnv:: TypeEnv -> FS [QSizeVar]
nonimpFromTyEnv tenv = 
  mapM (\(v,ty) -> nonImpFromTy ty) tenv >>= \nonimps ->
  return (concat nonimps)

-- result: list containing imperative size variables (all but size of array)
impFromTy:: AnnoType -> FS [QSizeVar]
impFromTy ty | isPrimitiveType ty = 
  unprimeThisTy ty >>= \uty ->
  return uty
impFromTy ty@ArrayType{} =
  sizeVarsFromAnnTy ty{indxTypes=[]} >>= \svs ->
  return $ map (\s -> (s,Unprimed)) svs

-- result: list containing non-imperative size variables (size of array)
nonImpFromTy:: AnnoType -> FS [QSizeVar]
nonImpFromTy ArrayType{elemType=eTy,indxTypes=iTys} =
  let qsvIndx = map (\PrimInt{anno=Just ann} -> (SizeVar ann,Unprimed)) iTys in
  return (qsvIndx)
nonImpFromTy _ = return []

initArrFormula:: AnnoType -> AnnoType -> FS (Maybe Formula)
initArrFormula tp arrTy = 
  isIndirectionArrTy arrTy >>= \isIndir ->
  case arrTy of
    ArrayType{elemType=eTy,indxTypes=iTys} ->
      if not (sameBaseTy tp eTy) then return Nothing
      else
        impFromTy arrTy >>= \arrtys ->
        if isIndir then
          let elemBounds = arrtys in
          let primedBounds = primeTheseQSizeVars elemBounds in
          case tp of
            PrimInt{anno=Just a} ->
                let formula = fAnd $ map (\bnd -> EqK [Coef bnd (1),Coef (SizeVar a,Unprimed) (-1)]) primedBounds in
                return (Just formula)
            PrimFloat{} -> return (Just fTrue)
            _ -> return Nothing
        else 
          return $ Just (noChange arrtys)

--used in DeclArray rule
--appends a counter to the label: label corresponding to dimension 1, dim 2,...
genLabelArr:: (Maybe Label) -> Int -> QLabel
genLabelArr (Just lbl) cnt = lbl:['D':show cnt]
genLabelArr Nothing cnt = error $ "no label for array declaration: type annotation process not done??"
