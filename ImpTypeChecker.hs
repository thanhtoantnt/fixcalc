{- | Provides a type-checking function.

  Checks that callee preconditions are satisfied at each call-site, 
  and that the collected postcondition implies the one given in the method declaration.
-}
module ImpTypeChecker(typeCheckProg) where
import Fresh(runFS,FS(),initialState,fresh,addOmegaStr,getFlags,putStrFS,getCPUTimeFS,
    incSafePrimChecks,incUnsafePrimChecks,getSafePrimChecks,getUnsafePrimChecks,incUnsafeUserChecks,getSafeUserChecks,getUnsafeUserChecks)
import ImpAST 
import ImpConfig(Flags,Postcondition(..),postcondition)
import ImpFormula
import ImpTypeCommon(setsForParamPassing,equate,freshTy,genLabelArr,initArrFormula,
   TypeEnv,extendTypeEnv,lookupVar,impFromTyEnv,impFromTy,initialTransFromTyEnv,initialTransFromTy)
import MyPrelude
-----------------
import Data.List(union,(\\),nub)
import Data.Maybe(fromJust)
import Control.Monad(when)

-------TypeChecking----------------
typeCheckProg:: Prog -> FS Prog
typeCheckProg dsgProg@(Prog _ dsgPrims dsgMeths) = 
  getCPUTimeFS >>= \time1 ->
  addOmegaStr "# Starting checking..." >>
  mapM (typeCheckMethDecl dsgProg) dsgMeths >>
  mapM (typeCheckPrimDecl dsgProg) dsgPrims >>
  addOmegaStr "# Checking is finished..." >>
--    printProgImpp dsgProg >>
  getCPUTimeFS >>= \time2 ->
  getSafePrimChecks >>= \safePrim -> getUnsafePrimChecks >>= \unsafePrim -> getUnsafeUserChecks >>= \unsafeUser -> 
  let totalPrim = safePrim+unsafePrim in
  let unsafe = unsafePrim+unsafeUser in
--  putStrFS ("(TotalPrim, SafePrim, Unsafe) = (" ++ show totalPrim ++", "++show safePrim++", "++show unsafe++")") >>
  putStrFS ("Pre/Post checking...done in " ++ showDiffTimes time2 time1) >> 
  return dsgProg

-------Meth-Declare----------------
typeCheckMethDecl:: Prog -> MethDecl -> FS ()
typeCheckMethDecl prog m@MethDecl{methPres=pres,methPost=(post:_),methBody=eb} =
  let ((_,t,fname):args) = methParams m in
  addOmegaStr ("# Checking for " ++ fname) >>
  (setsForParamPassing (Meth m)) >>= \(v,_,w,_,qsvByVal) ->
  let fqsvFormulae = fqsv post `union` concatMap (\(lbl,pre) -> fqsv pre) pres in
  let qsvTypes = w `union` v in
  let fqsvTypes = qsvTypes `union` primeTheseQSizeVars qsvTypes in
  if not (null (fqsvFormulae \\ fqsvTypes)) then
    error $ "unbounded size variable in method header:\n" ++ show (fqsvFormulae \\ fqsvTypes) ++ "\n " ++ showImpp m
  else
    let psi = noChange qsvByVal in
    let gamma = map (\(_,tyi,vi) -> (vi,tyi)) args in
    initialTransFromTyEnv gamma >>= \deltaInit ->
    let delta1 = fAnd (deltaInit:snd (unzip pres)) in -- all pres are assumed to be true!
    typeCheckExp prog eb fname gamma delta1 >>= \(tp,delta2) ->
    rename tp t >>= \subst ->
    case subst of
      Nothing -> error $ "incompatible types\nfound "++showImpp tp++ "\nrequired: "++showImpp t++"\n "++showImpp m
      Just rho ->
            debugApply rho delta2 >>= \delta2p ->
            let infPost = fExists (primeTheseQSizeVars qsvByVal) delta2p in
----check resulting postcondition
            addOmegaStr ("# Postcondition check ") >>
            safeChk [] infPost (False,"","","") (["POST",fname],post) >>= \satisfied ->
            if not satisfied then 
              putStrFS ("ERROR: postcondition possibly not established for function " ++ fname)
            else return ()
----check resulting postcondition

-------Prim-Declare----------------
typeCheckPrimDecl:: Prog -> PrimDecl -> FS ()
typeCheckPrimDecl prog p@PrimDecl{primPres=pres,primPost=post} = 
  let ((_,t,fname):args) = primParams p in
  setsForParamPassing (Prim p) >>= \(v,_,w,_,qsvByVal) ->
  let fqsvFormulae = fqsv post `union` concatMap (\(lbl,pre) -> fqsv pre) pres in
  let qsvTypes = w `union` v in
  let fqsvTypes = qsvTypes `union` primeTheseQSizeVars qsvTypes in
  if not (null (fqsvFormulae \\ fqsvTypes)) then
    error $ "unbounded size variable in primitive header:\n" ++ show (fqsvFormulae \\ fqsvTypes) ++ "\n " ++ showImpp p
  else
    let quanX = fExists (primeTheseQSizeVars qsvByVal) post in
    return ()

-----Expressions-----------------
typeCheckExp:: Prog -> Exp -> Lit -> TypeEnv -> Formula -> FS (AnnoType,Formula)
-- ^The computation 'typeCheckExp' prog exp mn gamma delta = FS (tau,delta1) resembles the judgement: prog,gamma,delta,mn |- exp::tau,delta1
-------KTrue-----------------------
typeCheckExp prog KTrue mn gamma delta = 
  fresh >>= \s ->
  let ty = PrimBool{anno=Just s} in 
  let ctx = And [delta,EqK[Coef (SizeVar s,Unprimed) (-1),Const 1]] in
  return (ty,ctx)
-------KFalse----------------------
typeCheckExp prog KFalse mn gamma delta = 
  fresh >>= \s ->
  let ty = PrimBool{anno=Just s} in 
  let ctx = And [delta,EqK[Coef (SizeVar s,Unprimed) (-1),Const 0]] in
  return (ty,ctx)
-------KInt------------------------
typeCheckExp prog (KIntNum n) _ _ delta = 
  fresh >>= \s ->
  return (PrimInt{anno=Just s},And [delta,EqK[Coef (SizeVar s,Unprimed) (-1),Const n]])
-------KFloat----------------------
typeCheckExp prog (KFloatNum f) _ _ delta = 
  return (PrimFloat{anno=Nothing},delta)
-------KVoid-----------------------
typeCheckExp prog (KVoid) _ _ delta = 
  return (PrimVoid{anno=Nothing},delta)
-------Var-------------------------
typeCheckExp prog exp@(ExpVar lit) mn gamma delta = 
  case lookupVar lit gamma of
    Nothing -> error $ "undefined variable " ++ lit ++ "\n in function " ++ mn
    Just ty -> freshTy ty >>= \typ ->
      equate (typ,ty) (Unprimed,Primed) >>= \(Just phi) ->
      let deltap = fAnd [delta,phi] in
      return (typ,deltap)
-------VarAssign-------------------
typeCheckExp prog exp@(AssignVar lit e1) mn gamma delta =
  case lookupVar lit gamma of
    Nothing -> error $ "undefined variable " ++ lit ++ "\n "++showImppTabbed exp 1++"\n in function " ++ mn
    Just ty ->
      typeCheckExp prog e1 mn gamma delta >>= \(ty1,delta1) ->
      equate (ty,ty1) (Primed,Unprimed) >>= \subst ->
      case subst of
        Nothing -> error $ "incompatible types\nfound: " ++ showImpp ty1 ++ "\nrequired: " ++ showImpp ty ++ "\n "++showImppTabbed exp 1
        Just phi ->
              fsvTy ty1 >>= \x ->
              impFromTy ty >>= \u ->
              composition u delta1 phi >>= \deltaA ->
              return (PrimVoid{anno=Nothing},fExists x deltaA) 

-------Seq-------------------------
typeCheckExp prog (Seq e1 e2) mn gamma delta = 
  typeCheckExp prog e1 mn gamma delta >>= \(ty1,delta1) ->
  fsvTy ty1 >>= \x ->
  typeCheckExp prog e2 mn gamma (fExists x delta1) >>= \(ty2,delta2) ->
  return $ (ty2,delta2)

-------If--------------------------
typeCheckExp prog exp@(If nonDet (ExpVar lit) exp1 exp2) mn gamma delta = 
  let bty = case lookupVar lit gamma of
        Nothing -> error $ "undefined variable " ++ lit ++ "\n "++showImppTabbed exp 1++"\n in function " ++ mn
        Just ty@PrimBool{} -> ty
        Just ty -> error $ "incompatible types in conditional test\nfound: "++showImpp ty++"\nrequired: Bool\n "++showImppTabbed exp 1 in
    case bty of
      PrimBool{anno=Just b} ->
        let qb = (SizeVar b,Primed) in
-- Hai improvemed his type checker using a simplification of the delta-context at this point.
-- For IMP, this generates significant less checking time only for LU (50% - from 195s to 84s)
-- Point to do simplification? 
        simplify delta >>= \delta ->
        let deltab1 = fAnd [delta,EqK [Coef qb (-1),Const 1]] in
        let deltab0 = fAnd [delta,EqK [Coef qb 1]] in 
            typeCheckExp prog exp1 mn gamma deltab1 >>= \(ty1,delta1) ->
            typeCheckExp prog exp2 mn gamma deltab0 >>= \(ty2,delta2) ->
                (case (ty1,ty2) of
                  (TopType{},TopType{}) -> error $ "typeCheck: both branches of conditional contain error constructs"++showImppTabbed exp 1
                  (TopType{},_) -> freshTy ty2
                  (_,_) -> freshTy ty1) >>= \ty ->
                  rename ty1 ty >>= \(Just rho1) -> --can't fail
                  rename ty2 ty >>= \subst2 ->
                  case subst2 of
                    Nothing -> error $ "incompatible types in branches of conditional\nfound: "++showImpp ty1++"\nand: "++showImpp ty2++"\n "++showImppTabbed exp 1
                    Just rho2 ->
                      debugApply rho1 delta1 >>= \rho1Delta1 ->
                      debugApply rho2 delta2 >>= \rho2Delta2 ->
                          let deltap = Or [rho1Delta1,rho2Delta2] in
                            return (ty,deltap)
      PrimBool{anno=Nothing} ->
        error $ "no annotation for the test value in conditional"++showImppTabbed exp 1
typeCheckExp prog exp@(If nonDet _ exp1 exp2) mn gamma delta = error $ "test in conditional is not a variable\nDesugaring is not done before type checking?"++showImppTabbed exp 1
-------Empty Block-----------------
typeCheckExp prog (ExpBlock [] exp1) mn gamma delta = 
  typeCheckExp prog exp1 mn gamma delta

-------Block1-DeclVar--------------
typeCheckExp prog exp@(ExpBlock [VarDecl ty lit exp1] exp2) mn gamma delta = 
  typeCheckExp prog exp1 mn gamma delta >>= \(ty1,delta1) ->
  impFromTyEnv gamma >>= \u ->
  initialTransFromTy ty >>= \psi ->
  equate (ty1,ty) (Unprimed,Primed) >>= \eqF ->
  case eqF of
    Nothing -> error $ "incompatible types\nfound: " ++ showImpp ty1 ++ "\nrequired: " ++ showImpp ty ++ "\n "++ showImppTabbed exp 1
    Just equ ->
          let delta1p = fAnd [delta1,equ] in
          let extGamma = extendTypeEnv gamma (lit,ty) in
          fsvTy ty1 >>= \x ->
          typeCheckExp prog exp2 mn extGamma (And  [fExists x delta1p,psi]) >>= \(ty2,delta2) ->
          fsvTy ty >>= \svtys ->
          impFromTy ty >>= \isvtys ->
          let y = svtys `union` primeTheseQSizeVars isvtys in
          return (ty2,fExists y delta2)

-------Block2-DeclArr--------------
typeCheckExp prog exp@(ExpBlock [LblArrVarDecl lbl ty indxs lit exp1] exp2) mn gamma delta = 
  case ty of
    ArrayType{indxTypes=iTys} ->
      let lits = map(\i -> case i of 
                              ExpVar lit -> lit
                              _ -> error $ "incompatible expressions in array declaration\n found: " ++ showImppTabbed i 1 ++ "\nrequired: variable.\n "++showImppTabbed exp 1
                    ) indxs in
      let tyvs = map (\l -> case lookupVar l gamma of
                    Nothing -> error $ "undefined variable " ++ lit ++ "\n "++showImppTabbed exp 1++"\n in function " ++ mn
                    Just ty@PrimInt{} -> ty
                    Just ty -> error $ "incompatible types in array declaration - indexes must be integers\nfound: "++showImpp ty++"\nrequired: Int\n "++showImppTabbed exp 1
            ) lits in
      let sisPair = map (\tyv -> case tyv of {
                              PrimInt{anno=Just s} -> ((SizeVar s,Unprimed),(SizeVar s,Primed));
                              _ -> error $ "variable used for initialization of dimensions of array is not annotated: " ++ showImpp tyv}
                    ) tyvs in
      let (sisU,sisP) = unzip sisPair in
      -- check same no of dimensions
      if not (length iTys == length tyvs) then 
        error $ "incompatible no. of dimension is array declaration: " ++ concatSepBy "," (map showImpp iTys) ++ " and " ++ concatSepBy "," (map showImpp tyvs) ++ "\n "++showImppTabbed exp 1
      else
      -- check same type for each dimension: should be TyInt
      let sGT0sUnprimed = map (\si -> fGT[Coef si 1]) sisU in
      let sGT0sPrimed = map (\si -> fGT[Coef si 1]) sisP in
      mapM (\(n,s) -> (equate (n,s) (Unprimed,Primed) >>= \eqF ->
                          case eqF of {
                            Nothing -> error $ "incompatible types\nfound: " ++ showImpp s ++ "\nrequired: " ++ showImpp n ++ "\n "++ showImppTabbed exp 1;
                            Just equ -> return equ})
                      ) (zip iTys tyvs) >>= \nEqs -> -- no need for zipOrFail
      let checks = map (\(sGT0,cnt) -> (genLabelArr lbl cnt,sGT0)) (zip sGT0sUnprimed (enumFrom 1)) in -- no need for zipOrFail
      typeCheckExp prog exp1 mn gamma delta >>= \(tp,delta1) ->
      -- check init value is of the same type as eTy
      initArrFormula tp ty >>= \initFormula ->
      case initFormula of
        Nothing -> error $ "incompatible types in array declaration\nfound: " ++ showImpp tp ++ "\nrequired: " ++ showImpp ty ++ "\n " ++showImppTabbed exp 1
        Just indirPsi ->
          let fstComp = fAnd (indirPsi:[delta1]) in
          let sndComp = fAnd (sGT0sPrimed++nEqs) in
          let gammap = extendTypeEnv gamma (lit,ty) in
          impFromTyEnv gammap >>= \u ->
          let delta1p = fAnd[fstComp,sndComp] in
            addOmegaStr ("# During checking: declaration of array " ++ lit) >>
            safeChks u delta1 (True,showImppTabbed (LblArrVarDecl lbl ty indxs lit exp1) 1,mn,"arraydecl") checks >>
              fsvTy tp >>= \x ->
              fsvTy ty >>= \svty -> impFromTy ty >>= \isvty ->
              let y = svty `union` primeTheseQSizeVars isvty in
                typeCheckExp prog exp2 mn gammap (fExists x delta1p) >>= \(ty2,delta2) ->
                  return $ (ty2,fExists y delta2)
    _ -> error $ "incompatible types\n found: " ++ showImpp ty ++ "\nrequired: array type in declaration of " ++ lit ++ "\n "++showImppTabbed exp 1

typeCheckExp prog exp@(ExpBlock varDecls e1) mn gamma delta = 
  error $ "MultiDecls in ExpBlock - program is not desugared? \n function: " ++ mn ++"\n "++ showImppTabbed exp 1
-------Error-----------------------
typeCheckExp prog (ExpError) mn gamma delta = 
  return (TopType{anno=Nothing},fFalse)
-------Call------------------------
typeCheckExp (Prog _ prims meths) exp@(LblMethCall lbl fName argsIdent) mn gamma delta =
  addOmegaStr ("# During checking: call to " ++ fName) >>
  let getArgsTypes = \argIdent -> 
        case argIdent of
          ExpVar lit -> case lookupVar lit gamma of{Just ty -> ty;Nothing -> error $ "undefined variable " ++ lit ++ "\n "++showImppTabbed exp 1++"\n in function " ++ mn}
          arg -> error $ "Please use variables as arguments to primitive functions instead of\n : " ++ showImppTabbed arg 1 ++ "\n "++showImppTabbed exp 1
  in
  let typs = map getArgsTypes argsIdent in 
  impFromTyEnv gamma >>= \u ->
  concatMapM impFromTy typs >>= \wWithDup -> let w = nub wWithDup in
      let callables = map (\p -> Prim p) prims ++ map (\m -> Meth m) meths in
      let calleeDef = findCallable fName callables in
      (case calleeDef of
            Nothing -> error $ "call to undefined function " ++ fName ++"\n "++showImppTabbed exp 1
            Just (Meth m) -> 
              setsForParamPassing (fromJust calleeDef) >>= \(_,_,_,_,qsvByVal) ->
              let delta = map (\ctx -> fAnd [ctx,noChange qsvByVal]) (methPost m) in
              return (methParams m,strong $ delta,methPres m)
            Just (Prim p) -> 
              getFlags >>= \flags -> 
              if postcondition flags == StrongPost then 
                let strongPost = And ((primPost p):(map (\(lbl,f) -> f) (primPres p))) in
                return (primParams p,strongPost,primPres p)
              else return (primParams p,primPost p,primPres p)
        ) >>= \(idTys,deltam,phim) ->
        let ty = snd3 (head idTys) in
        freshTy ty >>= \typ ->
        if (length (snd3 (unzip3 idTys)) /= length (typ:typs)) then
          error $ "call to function " ++ fName ++ " with incompatible argument types\n "++showImppTabbed exp 1
        else
        let zipped = zip (snd3 (unzip3 idTys)) (typ:typs) in
        concatMapM (\(t,tp) -> rename t tp >>= \subst -> case subst of {
                      Just rho -> return rho;
                      Nothing -> error $ "incompatible types\nfound "++showImpp tp++ "\nrequired: "++showImpp t++"\n "++showImppTabbed exp 1;}
              ) zipped >>= \rho -> 
        debugApply rho deltam >>= \rhoDeltam ->
-- Point to do simplification? 
        simplify delta >>= \delta ->
        mapM (\(lbl,f) -> debugApply rho f >>= \rhoF -> return (lbl,rhoF)) phim >>= \rhoPhim ->
        safeChks u delta (True,showImppTabbed exp 1,mn,fName) rhoPhim >>
        composition w delta rhoDeltam >>= \delta2 ->
        return (typ,delta2)
        
-------ExpBogus--------------------
typeCheckExp prog ExpBogus mn gamma delta =
  error $ "ExpBogus: variable declaration without initialization??\n in function: " ++ mn

safeChk:: [QSizeVar] -> Formula -> (Bool,String,String,String) -> LabelledFormula -> FS Bool
safeChk u delta (isPreCheck,str1,str2,str3) lblChk = 
	let (qlbl,chk) = (fst lblChk,snd lblChk) in
  let lbl = last qlbl in
  let lblstr = showImpp qlbl in
	addOmegaStr ("# Check for " ++ lblstr ++ ": CTX subset PHI?") >>
  ctxImplication u delta chk >>= \isok ->
  case (not isok,isPreCheck) of 
    (True,True) -> --UNSAFE
      putStrFS ("ERROR: precondition (" ++ lblstr ++ ") is not satisfied at call site " ++ str1 ++ " (CTX_" ++ str2 ++ " => PRE_" ++ str3 ++")") >>
      when (lbl == "lPost" || lbl == "NEVER_BUG") incUnsafeUserChecks >>
      when ((head lbl == 'L') || (head lbl == 'H') || (head lbl == 'D')) incUnsafePrimChecks >>
      return isok
    (False,True) -> --SAFE
--        putStrFS ("precondition (" ++ lblstr ++ ") is satisfied at call site " ++ str1 ++ " (CTX_" ++ str2 ++ " => PRE_" ++ str3 ++")") >>
      when ((head lbl == 'L') || (head lbl == 'H') || (head lbl == 'D')) incSafePrimChecks >>
      return isok
    (_,False) -> -- for a postcondition, error message is shown in the caller
      return isok

safeChks:: [QSizeVar] -> Formula -> (Bool,String,String,String) -> [LabelledFormula] -> FS Bool
safeChks u delta strs lblChks = mapM (safeChk u delta strs) lblChks >>= \oks -> return (and oks)
  
