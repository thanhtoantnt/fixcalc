{- |It computes the call graph using functions from Data.Graph.
   
   The type-inference process traverses the call-graph in bottom-up order and analyzes each method.
   
   The functions mkChk and mkChkRec decide whether a check is safe, unsafe or partially-safe.
-}
module ImpTypeInfer(methAdjacencies,getExternalMethods,setExternalMethods,typeInferSccs) where
import Fresh
import ImpAST
import ImpConfig
import ImpFormula
import ImpFixpoint2k(fixpoint2k)
import ImpOutInfer(RecFlags)
import ImpTypeCommon
import MyPrelude
-----------------
import Data.Array(assocs)
import Data.Graph(SCC(..),stronglyConnComp,graphFromEdges,indegree)
import Data.List(union,unzip4,(\\),nub)
import Data.Maybe(catMaybes,fromJust)
import Control.Monad(when)


typeInferSccs:: Prog -> [SCC Node] -> FS Prog
typeInferSccs prog [] = return prog
typeInferSccs prog (scc:sccs) = 
  typeInferScc prog scc >>= \updProg ->
  typeInferSccs updProg sccs

typeInferScc:: Prog -> SCC Node -> FS Prog
typeInferScc prog scc =
  case scc of
    AcyclicSCC mDecl ->
      putStrFS ("Inferring " ++ methName mDecl ++ "...") >> getCPUTimeFS >>= \time1 ->
      typeInferMethDeclNonRec prog mDecl >>= \updProg -> 
      getCPUTimeFS >>= \time2 ->
      putStrFS ("Inferring " ++ methName mDecl ++ "...done in " ++ showDiffTimes time2 time1) >> 
      return updProg
    CyclicSCC mDecls ->
      if (length mDecls /= 1) then error "Mutual recursion is not implemented"
      else
        let mDecl = (head mDecls) in
        putStrFS ("Inferring " ++ methName mDecl ++ "...") >> getCPUTimeFS >>= \time1 ->
        typeInferMethDeclRec prog mDecl >>= \updProg -> 
        getCPUTimeFS >>= \time2 ->
        putStrFS ("Inferring " ++ methName mDecl ++ "...done in " ++ showDiffTimes time2 time1) >> 
        return updProg

typeInferMethDeclRec:: Prog -> MethDecl -> FS Prog
typeInferMethDeclRec prog m =
  getFlags >>= \flags ->  
  let ((passbyM,t,fname):args) = methParams m in
  addOmegaStr ("# Inference for recursive " ++ fname) >>
  setsForParamPassing (Meth m) >>= \(inputs,outputs,res,qsvByRef,qsvByVal) ->
  let gamma = map (\(_,tyi,vi) -> (vi,tyi)) args in initialTransFromTyEnv gamma >>= \deltaInit ->
--phase 1
  typeInferExp prog (methBody m) fname inputs gamma (triple deltaInit) (1,[fname]) >>= \(tp,delta,_,_) ->
  rename tp t >>= \maybeRho ->
  case maybeRho of
    Nothing -> error $ "incompatible types\nfound "++showImpp tp++ "\nrequired: "++showImpp t++"\n "++showImpp m
    Just rho ->
      mapM (debugApply rho) delta >>= \deltap ->
      let delta1 = map (\ctx -> Exists (primeTheseQSizeVars qsvByVal) ctx) deltap in
      let delta2 = case postcondition flags of { WeakPost -> weak delta1; StrongPost -> strong delta1} in
      let recPost = RecPost fname delta2 (inputs,outputs,qsvByVal) in
        fixpoint2k m recPost >>= \(fixedPost,fixedInv) ->
        applyRecToPrimOnInvariant fixedInv >>= \inv ->
        let preFromPost = if prederivation flags == PostPD 
                          then [(["lPost"],fExists outputs fixedPost)]
                          else [] in
        let fixedM = m{methPost=(triple fixedPost),methInv=inv,methPres=preFromPost} in
        let fixedProg = updateMethDecl prog fixedM in
          let deltaInit1 = fAnd (deltaInit:snd (unzip preFromPost)) in -- preFromPost is assumed to be true!
          typeInferExp fixedProg (methBody fixedM) fname inputs gamma (triple deltaInit1) (2,[fname]) >>= \(_,_,newPres,newUpsis) ->
          return (updateMethDecl fixedProg (fixedM{methPres=preFromPost++newPres,methUpsis=newUpsis}))

typeInferMethDeclNonRec:: Prog -> MethDecl -> FS Prog
typeInferMethDeclNonRec prog m =
  let ((passbyM,t,fname):args) = methParams m in
  addOmegaStr ("# Inference for " ++ fname) >>
  setsForParamPassing (Meth m) >>= \(v,outputs,_,qsvByRef,qsvByVal) ->
  let gamma = map (\(_,tyi,vi) -> (vi,tyi)) args in initialTransFromTyEnv gamma >>= \deltaInit ->
  typeInferExp prog (methBody m) fname v gamma (triple deltaInit) (0,[]) >>= \(tp,delta,newPres,newUpsis) ->
  rename tp t >>= \maybeRho ->
  case maybeRho of
    Nothing -> error $ "incompatible types\nfound "++showImpp tp++ "\nrequired: "++showImpp t++"\n "++showImpp m
    Just rho -> 
          mapM (debugApply rho) delta >>= \deltap ->
          let delta1 = map (\ctx -> fExists (primeTheseQSizeVars qsvByVal) ctx) deltap in
          mapM simplify delta1 >>= \delta2 -> 
          getFlags >>= \flags -> 
          if prederivation flags == PostPD then 
            let preFromPost = [(["lPost"],fExists outputs (strong delta2))] in
            let fixedM = m{methPost=delta2,methInv=fTrue,methPres=preFromPost,methUpsis=[]} in
            let fixedProg = updateMethDecl prog fixedM in
            let deltaInit1 = fAnd (deltaInit:snd (unzip preFromPost)) in -- preFromPost is assumed to be true!
            typeInferExp fixedProg (methBody fixedM) fname v gamma (triple deltaInit1) (0,[]) >>= \(tp,delta,newPres,newUpsis) ->
            return (updateMethDecl fixedProg fixedM{methUpsis=newUpsis})
          else 
            return (updateMethDecl prog m{methPost=delta2,methInv=fTrue,methPres=newPres,methUpsis=newUpsis})

-- Program -> Exp -> Method_name -> Variables_to_Quantify -> Type_Environment -> RecFlags
-- -> (Type_for_exp,Context,Preconditions,RuntimeExps)
typeInferExp:: Prog -> Exp -> Lit -> [QSizeVar] -> TypeEnv -> [Formula] -> RecFlags 
  -> FS (AnnoType,[Formula],[LabelledFormula],[QLabel])
-------KTrue-----------------------
typeInferExp prog KTrue mn _ gamma delta recFlags = 
  fresh >>= \s ->
  let ty = PrimBool{anno=Just s} in
  let ctx = map (\context -> And [context,EqK[Coef (SizeVar s,Unprimed) (-1),Const 1]]) delta in
  return (ty,ctx,[],[])
-------KFalse----------------------
typeInferExp prog KFalse mn _ gamma delta recFlags = fresh >>= \s ->
  let ty = PrimBool{anno=Just s} in
  let ctx = map (\context -> And [context,EqK[Coef (SizeVar s,Unprimed) (-1),Const 0]]) delta in
  return (ty,ctx,[],[])
-------KInt------------------------
typeInferExp prog (KIntNum n) _ _ _ delta recFlags = fresh >>= \s ->
  let ty = PrimInt{anno=Just s} in
  let ctx = map (\context -> And [context,EqK[Coef (SizeVar s,Unprimed) (-1),Const n]]) delta in
  return (ty,ctx,[],[])
-------KFloat----------------------
typeInferExp prog (KFloatNum f) _ _ _ delta recFlags = 
  let ty = PrimFloat{anno=Nothing} in
  return (ty,delta,[],[])
-------KVoid-----------------------
typeInferExp prog (KVoid) _ _ _ delta recFlags = 
  let ty = PrimVoid{anno=Nothing} in
  return (ty,delta,[],[])
-------ExpBogus--------------------
typeInferExp prog ExpBogus mn _ _ _ recFlags =
  error $ "ExpBogus: variable declaration without initialization??\n in function: " ++ mn
-------Var-------------------------
typeInferExp prog (ExpVar lit) mn v gamma delta recFlags= 
  case lookupVar lit gamma of
    Nothing -> error $ "undefined variable " ++ lit ++ "\n in function " ++ mn
    Just ty -> freshTy ty >>= \typ ->
      equate (typ,ty) (Unprimed,Primed) >>= \(Just phi) ->
      let deltap = map (\context -> fAnd [context,phi]) delta in
      return (typ,deltap,[],[])
-------VarAssign-------------------
typeInferExp prog@(Prog _ _ meths) exp@(AssignVar lit e1) mn v gamma delta recFlags=
---- Disallow pass-by-val parameters to be assigned to:
--  let crtDef = findCallable mn (map (\m -> Meth m) meths) in
--  let params = case crtDef of
--        Just (Meth m) -> (methParams m)
--        Nothing -> error $ "assertion failed: crt-function definition not found\n " ++ showImppTabbed exp 1 in
--  if (fst3 (head params) == PassByVal) && any (\(p,ty,name) -> p==PassByVal && name==lit) (tail params) then 
--    error $ "assignment to pass-by-val parameter " ++ lit ++ "\n "++showImppTabbed exp 1++"\n in function " ++ mn
--  else
  case lookupVar lit gamma of
    Nothing -> error $ "undefined variable " ++ lit ++ "\n "++showImppTabbed exp 1++"\n in function " ++ mn
    Just ty ->
      typeInferExp prog e1 mn v gamma delta recFlags >>= \(ty1,delta1,phis,upsis) ->
      equate (ty,ty1) (Primed,Unprimed) >>= \subst ->
      case subst of
        Nothing -> error $ "incompatible types\nfound: " ++ showImpp ty1 ++ "\nrequired: " ++ showImpp ty ++ "\n "++ showImppTabbed exp 1
        Just phi ->
              fsvTy ty1 >>= \x ->
              impFromTy ty >>= \u ->
              mapM (\context -> composition u context phi) delta1 >>= \deltaA ->
              return (PrimVoid{anno=Nothing},map (fExists x) deltaA,phis,upsis) 

-------Seq-------------------------
typeInferExp prog (Seq e1 e2) mn v gamma delta recFlags = 
  typeInferExp prog e1 mn v gamma delta recFlags >>= \(ty1,delta1,phis1,upsis1) ->
    fsvTy ty1 >>= \x ->
      typeInferExp prog e2 mn v gamma (map (fExists x) delta1) recFlags >>= \(ty2,delta2,phis2,upsis2) ->
        let phis = phis1 `union` phis2 in
        let upsis = upsis1 `union` upsis2 in
        return $ (ty2,delta2,phis,upsis)

-------If--------------------------
typeInferExp prog exp@(If nonDet (ExpVar lit) exp1 exp2) mn v gamma delta recFlags = 
  let bty = case lookupVar lit gamma of
        Nothing -> error $ "undefined variable " ++ lit ++ "\n "++showImppTabbed exp 1++"\n in function " ++ mn
        Just ty@PrimBool{} -> ty
        Just ty -> error $ "incompatible types in conditional test\nfound: "++showImpp ty++"\nrequired: Bool\n "++showImppTabbed exp 1 in
    case bty of
      PrimBool{anno=Just b} ->
        let qb = (SizeVar b,Primed) in
-- deltaNoCond is used only if SelectivePD is enabled
        let deltab1 = map (\context -> fAnd [context,EqK [Coef qb (-1),Const 1]]) (take 3 delta) in
        let deltab0 = map (\context -> fAnd [context,EqK [Coef qb 1]]) (take 3 delta) in 
        getFlags >>= \flags -> 
        let (tripleDeltab1,tripleDeltab0) = 
              (if prederivation flags==SelectivePD then 
                --(cond delta) is used for more precise gisting in mkChk, but is expensive to compute (LU,linpack)
                ((take 2 deltab1)++[cond delta],(take 2 deltab0)++[cond delta])
               else (deltab1,deltab0)) in
        typeInferExp prog exp1 mn v gamma tripleDeltab1 recFlags >>= \(ty1,delta1,phis1,upsis1) ->
        typeInferExp prog exp2 mn v gamma tripleDeltab0 recFlags >>= \(ty2,delta2,phis2,upsis2) ->
            (case (ty1,ty2) of
              (_,TopType{}) -> error ("assertion failed: TyTop during typeInference. error?\n "++showImppTabbed exp 1)
              (TopType{},_) -> error ("assertion failed: TyTop during typeInference. error?\n "++showImppTabbed exp 1)
              (_,_) -> freshTy ty1) >>= \ty ->
              rename ty1 ty >>= \(Just rho1) -> --can't fail
              rename ty2 ty >>= \maybeRho ->
              case maybeRho of
                Nothing -> error $ "incompatible types in branches of conditional\nfound: "++showImpp ty1++"\nand: "++showImpp ty2++"\n "++showImppTabbed exp 1
                Just rho2 ->
                  mapM (debugApply rho1) delta1 >>= \rho1Delta1 ->
                  mapM (debugApply rho2) delta2 >>= \rho2Delta2 ->
                  let deltap = map (\(context1,context2) -> Or [context1,context2]) (zip rho1Delta1 rho2Delta2) in
                  let phis = phis1 `union` phis2 in
                  let upsis = upsis1 `union` upsis2 in
                    return (ty,deltap,phis,upsis)
      PrimBool{anno=Nothing} ->
        error $ "no annotation for the test value in conditional\n "++showImppTabbed exp 1
typeInferExp prog exp@(If nonDet _ exp1 exp2) mn v gamma delta recFlags = 
  error $ "test in conditional is not NQVar -- not implemented" ++ showImppTabbed exp 1

-------Empty Block-----------------
typeInferExp prog (ExpBlock [] exp1) mn v gamma delta recFlags = 
  typeInferExp prog exp1 mn v gamma delta recFlags 

-------Block1-DeclVar--------------
typeInferExp prog exp@(ExpBlock [VarDecl ty lit exp1] exp2) mn v gamma delta recFlags = 
  typeInferExp prog exp1 mn v gamma delta recFlags >>= \(ty1,delta1,phis1,upsis1) ->
  impFromTyEnv gamma >>= \u ->
  initialTransFromTy ty >>= \psi ->
  equate (ty1,ty) (Unprimed,Primed) >>= \subst ->
  case subst of
    Nothing -> error $ "incompatible types\nfound: " ++ showImpp ty1 ++ "\nrequired: " ++ showImpp ty ++ "\n "++ showImppTabbed exp 1
    Just equ ->
          let delta1p = map (\context -> fAnd[context,equ]) delta1 in
          let extGamma = extendTypeEnv gamma (lit,ty) in
          (fsvTy ty1) >>= \x ->
          let deltaP = map (\context -> And  [fExists x context,psi]) delta1p in
          typeInferExp prog exp2 mn v extGamma deltaP recFlags >>= \(ty2,delta2,phis2,upsis2) ->
          fsvTy ty >>= \svty -> impFromTy ty >>= \isvty ->
          let y = svty `union` primeTheseQSizeVars isvty in
          let phis = phis1 `union` phis2 in
          let upsis = upsis1 `union` upsis2 in
          return (ty2,map (fExists y) delta2,phis,upsis)

-------Block2-DeclArr--------------
typeInferExp prog exp@(ExpBlock [LblArrVarDecl lbl ty indxs lit exp1] exp2) mn v gamma delta recFlags = 
  case ty of
    ArrayType{elemType=elemTy,indxTypes=iTys} ->
      let lits = map(\i -> case i of 
            ExpVar lit -> lit
            _ -> error $ "incompatible expressions in array declaration\n found: " ++ showImppTabbed i 1 ++ "\nrequired: variable.\n") indxs in
      let tyvs = map (\l -> case lookupVar l gamma of
            Nothing -> error $ "undefined variable " ++ lit ++ " in function " ++ mn ++ "\n "++showImppTabbed exp 1
            Just ty@PrimInt{} -> ty
            Just ty -> error $ "incompatible types in array declaration - indexes must be integers\nfound: "++showImpp ty++"\nrequired: "++showTy PrimInt{anno=Nothing}++"\n "++showImppTabbed exp 1) lits in
      let sisPair = map (\tyv -> case tyv of {
                                PrimInt{anno=Just s} -> ((SizeVar s,Unprimed),(SizeVar s,Primed));
                                _ -> error $ "variable used for initialization of dimensions of array is not annotated: " ++ showImpp tyv ++ "\n "++showImppTabbed exp 1}
                    ) tyvs in
      let (sisU,sisP) = unzip sisPair in
      -- check same no of dimensions
      if not (length iTys == length tyvs) then 
        error $ "incompatible no. of dimensions in array declaration: " ++ concatSepBy "," (map showImpp iTys) ++ " and " ++ concatSepBy "," (map showImpp tyvs) ++ "\n "++showImppTabbed exp 1
      else
      -- check same type for each dimension: should be TyInt
      let sGT0sUnprimed = map (\si -> fGT[Coef si 1]) sisU in
      let sGT0sPrimed = map (\si -> fGT[Coef si 1]) sisP in
      mapM (\(n,s) -> equate (n,s) (Unprimed,Primed) >>= \eqF -> case eqF of {
                                  Nothing -> error $ "incompatible types\nfound: " ++ showImpp s ++ "\nrequired: " ++ showImpp n ++ "\n "++ showImppTabbed exp 1;
                                  Just equ -> return equ}
                      ) (zip iTys tyvs) >>= \nEqs -> -- no need for zipOrFail
      let checks = map (\(sGT0,cnt) -> (genLabelArr lbl cnt,sGT0)) (zip sGT0sUnprimed (enumFrom 1)) in -- no need for zipOrFail
      typeInferExp prog exp1 mn v gamma delta recFlags >>= \(tp,delta1,phis1,upsis1) ->
      -- check init value is of the same type as elemTy
      initArrFormula tp ty >>= \arrFormula ->
      case arrFormula of
        Nothing -> error $ "incompatible types in array declaration\nfound: " ++ showImpp tp ++ "\ndeclared array: " ++ showImpp ty ++ "\n " ++showImppTabbed exp 1
        Just indirPsi ->
          let fstComp = map (\context -> fAnd [indirPsi,context]) delta1 in
          let sndComp = fAnd (sGT0sPrimed++nEqs) in
          let gammap = extendTypeEnv gamma (lit,ty) in
          impFromTyEnv gammap >>= \u ->
          let delta1p = map (\context -> fAnd[context,sndComp]) fstComp in
            addOmegaStr ("# During inference: declaration of array " ++ lit) >>
            initialTransFromTyEnv gamma >>= \invFromGamma ->
            mkChks v u delta1 invFromGamma checks >>= \(phisp,errs) ->
              fsvTy tp >>= \x ->
              fsvTy ty >>= \svty -> 
              impFromTy ty >>= \isvty ->
              let y = svty `union` primeTheseQSizeVars isvty in
                typeInferExp prog exp2 mn v gammap (map (fExists x) delta1p) recFlags >>= \(ty2,delta2,phis2,upsis2) ->
                let phis = phis1 `union` phisp `union` phis2 in
                let upsis = upsis1 `union` upsis2 `union` errs in
                  return (ty2,map (fExists y) delta2,phis,upsis)
    _ -> error $ "incompatible types\n found: " ++ showImpp ty ++ "\nrequired: array type in declaration of " ++ lit ++ "\n "++showImppTabbed exp 1

typeInferExp prog exp@(ExpBlock varDecls e1) mn v gamma delta recFlags = 
  error $ "MultiDecls in ExpBlock - program is not desugared? \n function: " ++ mn ++ "\n " ++ showImppTabbed exp 1
    
-------Call------------------------
typeInferExp (Prog _ prims meths) exp@(LblMethCall (Just lbl) fName argsIdent) 
  mn v gamma delta (wPhase,sccFs) =
  addOmegaStr ("# During inference: call to " ++ fName) >>
  let getArgsTypes = \argIdent -> 
        case argIdent of
          ExpVar lit -> case lookupVar lit gamma of{Just ty -> ty;Nothing -> error $ "undefined variable " ++ lit ++ " in function " ++ mn ++ "\n "++showImppTabbed exp 1}
          arg -> error $ "found non-variable as arguments to primitive function\n:" ++ showImppTabbed arg 1++ "\n "++showImppTabbed exp 1
    in
    let argsTyps = map getArgsTypes argsIdent in 
    impFromTyEnv gamma >>= \u ->
    concatMapM impFromTy argsTyps >>= \wWithDup -> let w = nub wWithDup in
    qsvFromTyEnv gamma >>= \uRec -> -- to derive preRec, nonImp must be in U
    let callables = map (\p -> Prim p) prims ++ map (\m -> Meth m) meths in
    let calleeDef = findCallable fName callables in
    (case calleeDef of
          Nothing -> error $ "call to undefined function " ++ fName ++ "\n " ++ showImppTabbed exp 1
          Just (Meth m) -> 
            setsForParamPassing (fromJust calleeDef) >>= \(_,_,_,_,qsvByVal) ->
            let delta = map (\ctx -> fAnd [ctx,noChange qsvByVal]) (methPost m) in
            return (fst3(unzip3(methParams m)),snd3(unzip3(methParams m)),delta,methPres m)
          Just (Prim p) -> 
            let strongPost = And ((primPost p):(map (\(lbl,f) -> f) (primPres p))) in
            let triplePost = [strongPost,primPost p,primPost p] in
            return (fst3(unzip3(primParams p)),snd3(unzip3(primParams p)),triplePost,primPres p)
    ) >>= \(formalPassBy,formalTyps,deltam,phim) ->
    freshTy (head formalTyps) >>= \typ ->
    let actualTyps = typ:argsTyps in
    if (length formalTyps /= length actualTyps) then
      error $ "call to function " ++ fName ++ " with incompatible argument types\n "++showImppTabbed exp 1
    else
    let zipped = zip formalTyps actualTyps in
    concatMapM(\(t,tp) -> rename t tp >>= \subst -> case subst of {
                      Just rho -> return rho;
                      Nothing -> error $ "incompatible types\nfound "++showImpp tp++ "\nrequired: "++showImpp t++"\n "++showImppTabbed exp 1;}
              ) zipped >>= \rho ->
    mapM (\(lbls,f) -> debugApply rho f >>= \rhoF -> return (lbl:lbls,rhoF)) phim >>= \rhoPhim ->
    let isRecCall = fName `elem` sccFs in 
    case wPhase of
      0 -> -- caller (current funtion) is a non-recursive function
-- Point to do simplification? 
          mapM simplify delta >>= \delta ->
          initialTransFromTyEnv gamma >>= \invFromGamma ->
          mkChks v u delta invFromGamma rhoPhim >>= \(phis,upsis) ->
          mapM (debugApply rho) deltam >>= \rhoDeltam ->
          mapM (\(context1,context2) -> composition w context1 context2) (zip delta rhoDeltam) >>= \delta2 ->
          return (typ,delta2,phis,upsis)
      1 -> -- caller is recursive: gather recursive CAbst
          if isRecCall then 
            let zp = zip3 formalPassBy actualTyps (replicate (length actualTyps) undefined) in
            let methForSets = (case (fromJust calleeDef) of {Meth m -> m;_->error ""}){methParams=zp} in
            setsForParamPassing (Meth methForSets) >>= \(inputs,outputs,res,_,qsvByVal) ->
            let insouts = inputs `union` outputs in
            let delta1 = (And [noChange qsvByVal,AppRecPost fName insouts]) in
            mapM (\context -> composition w context delta1) delta >>= \delta2 ->
            return $ (typ,delta2,[],[]) -- when wPhase is 1, pres and upsis are not collected
          else
            mapM (debugApply rho) deltam >>= \rhoDeltam ->
            mapM (\(context1,context2) -> composition w context1 context2) (zip delta rhoDeltam) >>= \delta2 ->
            return $ (typ,delta2,[],[]) -- when wPhase is 1, pres and upsis are not collected
      2 -> -- caller is recursive: after fixpoint
          let crtName = (head sccFs) in -- assumes sccFs is singleton: no mutual recursion!!
          let crtDef = findCallable crtName callables in
          let (crtArgs,crtInv) = case crtDef of
                Just (Meth m) -> (methParams m,methInv m)
                Nothing -> error $ "assertion failed: function not-found at recursive call\n " ++ showImppTabbed exp 1 in
          let realTys = map (\(_,tyi,vi) -> tyi) crtArgs in
          if isRecCall then 
               getFlags >>= \flags -> 
              (if (prederivation flags == PostPD) then --if self-Recursive call and PostPD - enable checking!!
                initialTransFromTyEnv gamma >>= \invFromGamma ->
                mkChksRec v u uRec realTys delta crtInv invFromGamma rhoPhim >>= \(phis,upsis) -> return upsis
              else --if self-Recursive call - disable checking!!
                return []) >>= \newUpsis ->
              mapM (debugApply rho) deltam >>= \rhoDeltam ->
              mapM (\(context1,context2) -> composition w context1 context2) (zip delta rhoDeltam) >>= \delta2 ->
              return $ (typ,delta2,[],newUpsis) 
          else --derive preFst and preRec
-- Point to do simplification? 
              mapM simplify delta >>= \delta ->
              initialTransFromTyEnv gamma >>= \invFromGamma ->
              mkChksRec v u uRec realTys delta crtInv invFromGamma rhoPhim >>= \(phis,upsis) ->
              mapM (debugApply rho) deltam >>= \rhoDeltam ->
              mapM (\(context1,context2) -> composition w context1 context2) (zip delta rhoDeltam) >>= \deltap ->
              return $ (typ,deltap,phis,upsis)

typeInferExp prog exp@(ExpError) mn v gamma delta recFlags = 
  error $ "ExpError encountered during type inference?\n "++showImppTabbed exp 1
typeInferExp prog e _ _ _ _ _ = error $ "typeInferExp not implemented for the following construct\n " ++ showImppTabbed e 1

-------MkChk-----------------------
mkChks:: [QSizeVar] -> [QSizeVar] -> [Formula] -> Formula -> [LabelledFormula] -> FS ([LabelledFormula],[QLabel])
mkChks v u delta typeInv lblChks =
  mapM (mkChk v u delta typeInv) lblChks >>= \mays ->
  let (maybePhis,maybeUpsis) = unzip mays in
    return $ (catMaybes maybePhis,catMaybes maybeUpsis)

mkChk:: [QSizeVar] -> [QSizeVar] -> [Formula] -> Formula -> LabelledFormula -> FS (Maybe LabelledFormula,Maybe QLabel)
mkChk v u [deltaS,deltaW,deltaC] typeInv (lbl,phi) = 
  getFlags >>= \flags ->  
      if prederivation flags == PostPD then 
          addOmegaStr ("# Is " ++ showImpp lbl ++ " a total redundant check?") >> 
          ctxImplication u deltaS phi >>= \impliesT ->
          if impliesT then 
            return (Nothing,Nothing)
          else return (Nothing,Just lbl)
      else
  addOmegaStr ("# Is " ++ showImpp lbl ++ " a total redundant check?") >> 
  ctxImplication u deltaS phi >>= \impliesT ->
  if impliesT then return (Nothing,Nothing)
  else 
    mapM hull [deltaS,deltaW,deltaC] >>= \[deltaSH,deltaWH,deltaCH] ->
    let toGistWith = case prederivation flags of { WeakPD -> typeInv; StrongPD -> deltaSH; SelectivePD -> deltaCH} in
    let ctx = case postcondition flags of {WeakPost -> deltaWH; StrongPost -> deltaSH} in
    addOmegaStr ("# gist PHI given CTX ") >> ctxSimplify v u ctx phi toGistWith >>= \simple ->
  	addOmegaStr ("# gisted PHI subset False?") >> ctxImplication [] simple fFalse >>= \impliesF ->
    if impliesF then return (Nothing,Just lbl)
    else addOmegaStr ("# propagate gisted PHI") >> return (Just (lbl,simple),Nothing)

-------MkChkRec--------------------
mkChksRec:: [QSizeVar] -> [QSizeVar] -> [QSizeVar] -> [AnnoType] -> [Formula] -> Formula -> Formula -> [LabelledFormula] -> FS ([LabelledFormula],[QLabel])
mkChksRec v u uRec typs delta inv typeInv lblChks =
  mapM (mkChkRec v u uRec typs delta inv typeInv) lblChks >>= \mays ->
  let (maybePhis,maybeUpsis) = unzip mays in
    return $ (catMaybes maybePhis,catMaybes maybeUpsis)

mkChkRec:: [QSizeVar] -> [QSizeVar] -> [QSizeVar] -> [AnnoType] -> [Formula] -> Formula -> Formula -> LabelledFormula -> FS (Maybe LabelledFormula,Maybe QLabel)
mkChkRec v u uRec typs [deltaS,deltaW,deltaC] inv typeInv (lbl,phi) = 
  getFlags >>= \flags ->  
      if prederivation flags == PostPD then -- prederivation using necessary preconditions
          equateNonImpToPrim typs >>= \nonImpToPrim ->
          let ctxRec = fExists v (fAnd [deltaS,nonImpToPrim]) in
          addOmegaStr ("# Is " ++ showImpp lbl ++ " a total redundant check?") >> 
          ctxImplication uRec ctxRec phi >>= \impliesT ->
          if impliesT then 
            return (Nothing,Nothing)
          else return (Nothing,Just lbl)
      else
  equateNonImpToPrim typs >>= \nonImpToPrim ->
  let ctxRec = fExists v (fAnd [deltaS,nonImpToPrim]) in
  addOmegaStr ("# Is " ++ showImpp lbl ++ " a total redundant check?") >> 
  ctxImplication uRec ctxRec phi >>= \impliesT ->
  if impliesT then return (Nothing,Nothing)
  else
    mapM hull [deltaS,deltaW,deltaC] >>= \[deltaSH,deltaWH,deltaCH] ->
    let ctxRecS = fAnd [inv,fExists v (fAnd [deltaSH,nonImpToPrim])] in
    let ctxRecW = fAnd [inv,fExists v (fAnd [deltaWH,nonImpToPrim])] in
    let ctxRecC = fAnd [inv,fExists v (fAnd [deltaCH,nonImpToPrim])] in
    mapM simplify [ctxRecS,ctxRecW,ctxRecC] >>= \[ctxRecS,ctxRecW,ctxRecC] ->
    addOmegaStr ("# pR_cR = gist PHI_REC given CTX_REC") >> 
    let toGistWith = case prederivation flags of { WeakPD -> typeInv; StrongPD -> ctxRecS; SelectivePD -> ctxRecC} in
    let ctxRec = case postcondition flags of { WeakPost -> ctxRecW; StrongPost -> ctxRecS} in
    ctxSimplify v uRec ctxRec phi toGistWith >>= \simpleRecWithRec ->
    addOmegaStr ("# test the precondition: pR_cR && ctxFst => chk") >> 
    ctxImplication u (And [simpleRecWithRec,deltaSH]) phi >>= \phiUseful ->
    if phiUseful then
      let simple = simpleRecWithRec in
    	addOmegaStr ("# pR_cR subset False?") >> ctxImplication [] simple fFalse >>= \impliesF ->
      if impliesF then return (Nothing,Just lbl)
      else addOmegaStr ("# propagate pR_cR") >> return (Just (lbl,simple),Nothing)
    else
      if separateFstFromRec flags then
        addOmegaStr ("# pF_cF = gist PHI_FST given CTX_FST ") >> 
        let toGistWith = case prederivation flags of { WeakPD -> typeInv; StrongPD -> deltaSH; SelectivePD -> deltaCH} in
        let delta = case postcondition flags of { WeakPost -> deltaWH; StrongPost -> deltaSH } in
        ctxSimplify v u delta phi toGistWith >>= \simpleFstWithFst ->
        addOmegaStr ("# test the precondition: (pF_cF && pR_cR) && ctxFst => chk") >> 
        ctxImplication u (And [simpleFstWithFst,simpleRecWithRec,deltaSH]) phi >>= \phiUseful ->
        if phiUseful then --even if phiUseful is True, specialization may be needed to type check the recursive function
          let simple = fAnd [simpleFstWithFst,simpleRecWithRec] in
        	addOmegaStr ("# (pF_cF && pR_cR) subset False?") >> ctxImplication [] simple fFalse >>= \impliesF ->
          if impliesF then return (Nothing,Just lbl)
          else addOmegaStr ("# propagate (pF_cF && pR_cR)") >> return (Just (lbl,simple),Nothing)
        else
          addOmegaStr ("# derived precondition does not type-check; so I propagate false")>> 
          return (Nothing, Just lbl)
      else
        addOmegaStr ("# pF_tI = gist PHI_FST given TYPE_INV ") >> 
        let toGistWith = typeInv in
        let delta = case postcondition flags of { WeakPost -> deltaWH; StrongPost -> deltaSH } in
        ctxSimplify v u delta phi toGistWith >>= \simpleFstWithSta ->
        addOmegaStr ("# test the precondition: (pF_tI && pR_cR) && ctxFst => chk") >> 
        ctxImplication u (And [simpleFstWithSta,simpleRecWithRec,deltaSH]) phi >>= \phiUseful ->
        if phiUseful then
          let simple = fAnd [simpleFstWithSta,simpleRecWithRec] in
        	addOmegaStr ("# (pF_tI && pR_cR) subset False?") >> ctxImplication [] simple fFalse >>= \impliesF ->
          if impliesF then return (Nothing,Just lbl)
          else
              addOmegaStr ("# propagate (pF_tI && pR_cR)") >> return (Just (lbl,simple),Nothing)
        else
          addOmegaStr ("# derived precondition does not type-check; so I propagate false")>> 
          return (Nothing, Just lbl)

-------Meth SCC--------------------
type Key=Int
type Node=MethDecl

methAdjacencies:: [Node] -> [(Node, Key, [Key])]
methAdjacencies meths  = 
  let sccMeths = zip meths (enumFrom 0) in
    zip3 meths (enumFrom 0) (map (callees sccMeths) sccMeths)

callees:: [(MethDecl,Key)] -> (MethDecl,Key) -> [Key]
callees kmeths (m,k) =
  catMaybes $ map (getKeyForMeth kmeths) (getCalleesFromExp (methBody m))

getKeyForMeth:: [(MethDecl,Key)] -> Lit -> Maybe Key
getKeyForMeth kmeths lit = 
  let keys = catMaybes $ map (\(m,k) -> if getNameForMethDecl m == lit then Just k else Nothing) kmeths in
  case length keys of
    0 -> Nothing
    1 -> Just (head keys)
    _ -> error $ "More than one function defined with name: " ++ lit 

getCalleesFromExp:: Exp -> [Lit]
getCalleesFromExp exp = case exp of
  LblMethCall lbl id exps -> id : (concatMap getCalleesFromExp exps)
  AssignVar id exp -> getCalleesFromExp exp
  If nonDet test exp1 exp2 -> concatMap getCalleesFromExp [test,exp1,exp2]
  Seq exp1 exp2 -> concatMap getCalleesFromExp [exp1,exp2]
  ExpBlock varDecls exp -> (concatMap getCalleesFromDecl varDecls) ++ getCalleesFromExp exp
  _ -> []

getCalleesFromDecl:: VarDecl -> [Lit]
getCalleesFromDecl varDecl = case varDecl of
  VarDecl ty lit exp -> getCalleesFromExp exp
  LblArrVarDecl lbl ty indxs lit exp -> getCalleesFromExp exp
    
getNameForMethDecl:: MethDecl -> Lit
getNameForMethDecl m =
  let ((_,_,fname):_) = methParams m in fname

getExternalMethods:: Prog -> FS [MethDecl]
-- ^Returns those "external" methods that are not called from inside the given program.
getExternalMethods (Prog _ _ meths) = 
  return (filter methExternal meths)
  
setExternalMethods:: Prog -> FS Prog
setExternalMethods prog@(Prog _ _ meths) = 
  let graph = (fst3 (graphFromEdges (methAdjacenciesWithoutRec meths))) in
  let pairs = filter (\(ix,degree) -> degree == 0) (assocs (indegree graph)) in 
--  putStrFS ("External methods: " ++ concatMap (\(ix,degree) -> methName (meths !! ix)) pairs) >>
  let externalMeths = (map (\(ix,degree) -> (meths !! ix)) pairs) in
  return (updateProg prog externalMeths)
  where 
  updateProg:: Prog -> [MethDecl] -> Prog
  updateProg prog [] = prog
  updateProg prog (extm:extms) = 
    let newm = extm{methExternal=True} in
    let newprog = updateMethDecl prog newm in
    updateProg newprog extms

methAdjacenciesWithoutRec:: [Node] -> [(Node, Key, [Key])]
-- ^Same function as methAdjacencies, but does not count recursive calls.
methAdjacenciesWithoutRec meths  = 
  let sccMeths = zip meths (enumFrom 0) in
    zip3 meths (enumFrom 0) (map (calleesWithoutRec sccMeths) sccMeths)
  where
  calleesWithoutRec kmeths (m,k) =
    catMaybes $ map (getKeyForMeth kmeths) (fromExp (methName m) (methBody m))
  fromExp mname exp = case exp of
    LblMethCall lbl id exps -> let headd = if id==mname then [] else [id] in headd ++ (concatMap (fromExp mname) exps)
    AssignVar id exp -> fromExp mname exp
    If nonDet test exp1 exp2 -> concatMap (fromExp mname) [test,exp1,exp2]
    Seq exp1 exp2 -> concatMap (fromExp mname) [exp1,exp2]
    ExpBlock varDecls exp -> (concatMap (fromDecl mname) varDecls) ++ fromExp mname exp
    _ -> []
  fromDecl mname varDecl = case varDecl of
    VarDecl ty lit exp -> fromExp mname exp
    LblArrVarDecl lbl ty indxs lit exp -> fromExp mname exp

-- for debug: textual represetntation of SCCs
printSCCs:: [SCC Node] -> String
printSCCs [] = ""
printSCCs (n:ns) = (case n of
    AcyclicSCC v -> getNameForMethDecl v
    CyclicSCC vs -> "CYCLE " ++ concatMap getNameForMethDecl vs ++ " CYCLE"
  )
  ++"##"++printSCCs ns
