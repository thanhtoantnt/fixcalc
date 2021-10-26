{- | Does (simple) type-checking. At the same time, it converts loops (while, for) to tail-recursive functions.
 
 Checks for double declarations, incompatible types, use of undeclared variables.
-}
module ImpSTypeChecker(sTypeCheck) where
import ImpAST
import ImpConfig(isIndirectionIntArray)
import Fresh(FS,fresh,getFlags)
import ImpFormula(sameBaseTy)
import ImpTypeCommon(TypeEnv,extendTypeEnv,lookupVar,freshTy)
import MyPrelude(snd3,concatSepBy)
------------------------------------------
import Data.List(nub)

-----SimpleTypeChecking-------------------------------------------------
-----and conversion of While/For to tail-recursive functions------------
sTypeCheck:: Prog -> FS Prog
sTypeCheck prog@(Prog incls prims meths) = 
  let dupMeth = checkMethDuplicates meths in
  case dupMeth of
    Just m -> error $ "Double definition for method " ++ m ++ "\n"
    Nothing -> 
      mapM (sTypeCheckMethDecl prog) meths >>= \res ->
      mapM sTypeCheckPrimDecl prims >>= \primRes ->
      let primok = and primRes in
      if not primok then
        error $ ""
      else
        let (newMdss,newMeths) = unzip res in
        let allMeths = newMeths++(concat newMdss) in
        return (Prog incls prims allMeths)

sTypeCheckPrimDecl:: PrimDecl -> FS Bool
sTypeCheckPrimDecl p =
  getFlags >>= \flags ->  
  let ((_,retTy,fname):args) = primParams p in
  if (isIndirectionIntArray flags) && not (isPrimitiveType retTy) then
    error $ "Analysis of array indirection is enabled.\nReturn type of primitive "++fname++" must be a primitive type.\n"
  else 
    return True

sTypeCheckMethDecl:: Prog -> MethDecl -> FS ([MethDecl],MethDecl)
sTypeCheckMethDecl prog m =
  getFlags >>= \flags ->  
  let ((_,retTy,fname):args) = methParams m in
  if (isIndirectionIntArray flags) && not (isPrimitiveType retTy) then
    error $ "Analysis of array indirection is enabled.\nReturn type of any method must be a primitive type.\n" ++ showImppMethForChecking m
  else 
    let (_,annTys,lits) = unzip3 args in
    let gamma = zip lits annTys in
    sTypeCheckExp prog (methBody m) fname gamma >>= \(newMds,newEb,ty) ->
    if not (sameBaseTy ty retTy) then
      error $ "incompatible types\nfound "++showTy ty++ "\nrequired: "++showTy retTy++"\n "++showImppMethForChecking m
    else
      let newMDecl = m{methBody=newEb} in
      return (newMds,newMDecl)

checkMethDuplicates:: [MethDecl] -> Maybe String
checkMethDuplicates [] = Nothing
checkMethDuplicates (m:ms) = 
  if null (filter (\m2 -> methName m == methName m2) ms) then
    checkMethDuplicates ms
  else
    Just (methName m)

sTypeCheckExp:: Prog -> Exp -> Lit -> TypeEnv -> FS ([MethDecl],Exp,AnnoType)
sTypeCheckExp prog exp@KTrue mn gamma = return ([],exp,PrimBool{anno=Nothing})
sTypeCheckExp prog exp@KFalse mn gamma = return ([],exp,PrimBool{anno=Nothing})
sTypeCheckExp prog exp@(KIntNum n) mn gamma = return ([],exp,PrimInt{anno=Nothing})
sTypeCheckExp prog exp@(KFloatNum n) mn gamma = return ([],exp,PrimFloat{anno=Nothing})
sTypeCheckExp prog exp@KVoid mn gamma = return ([],exp,PrimVoid{anno=Nothing})
sTypeCheckExp prog exp@(ExpError) mn gamma = error $ "ExpError encountered during simple type checking?\n "++showImppTabbed exp 1
sTypeCheckExp prog exp@(ExpBogus) mn gamma = error $ "ExpBogus: variable declaration without initialization??\n"

sTypeCheckExp prog exp@(ExpVar lit) mn gamma =
  case lookupVar lit gamma of
    Nothing -> error $ "undefined variable " ++ lit ++ "\n in function " ++ mn
    Just ty -> return ([],exp,ty)

sTypeCheckExp prog exp@(AssignVar lit e1) mn gamma =
  sTypeCheckExp prog e1 mn gamma >>= \(newMds1,newE1,ty1) ->
  case lookupVar lit gamma of
    Nothing -> error $ "undefined variable " ++ lit ++ "\n in function " ++ mn
    Just ty -> 
      if not (sameBaseTy ty1 ty) then
        error $ "incompatible types\nfound: " ++ showTy ty1 ++ "\nrequired: " ++ showTy ty ++ "\n "++showImppTabbed exp 1
      else
        return (newMds1,(AssignVar lit newE1),PrimVoid{anno=Nothing})

sTypeCheckExp prog exp@(Seq e1 e2) mn gamma =
  sTypeCheckExp prog e1 mn gamma >>= \(newMds1,newE1,ty1) ->
  sTypeCheckExp prog e2 mn gamma >>= \(newMds2,newE2,ty2) ->
  return (newMds1++newMds2,Seq newE1 newE2,ty2)

sTypeCheckExp prog exp@(If nonDet e1 e2 e3) mn gamma = 
  sTypeCheckExp prog e1 mn gamma >>= \(newMds1,newE1,ty1) ->
  if not (sameBaseTy ty1 (PrimBool{anno=Nothing})) then
    error $ "incompatible types in conditional test\nfound: "++showTy ty1++"\nrequired: Bool\n "++showImppTabbed exp 1
  else  
    sTypeCheckExp prog e2 mn gamma >>= \(newMds2,newE2,ty2) ->
    sTypeCheckExp prog e3 mn gamma >>= \(newMds3,newE3,ty3) ->
    if not (sameBaseTy ty2 ty3) then
      error $ "incompatible types in branches of conditional\nfound: "++showTy ty2++"\nand: "++showTy ty3++"\n "++showImppTabbed exp 1
    else
      return (newMds1++newMds2++newMds3,If nonDet newE1 newE2 newE3,ty2)

sTypeCheckExp prog@(Prog incls prims meths) exp@(LblMethCall lbl fName args) mn gamma =
  mapM (\arg -> sTypeCheckExp prog arg mn gamma) args >>= \triples ->
  let (newMdss,newArgs,actualTys) = unzip3 triples in
  let newMds = concat newMdss in
  let callables = map (\p -> Prim p) prims ++ map (\m -> Meth m) meths in
  let calleeDef = findCallable fName callables in
  let ((_,retTy,_):formalArgs) = case calleeDef of
          Nothing -> error $ "call to undefined function " ++ fName ++ "\n " ++ showImppTabbed exp 1
          Just (Meth m) -> methParams m
          Just (Prim p) -> primParams p in
  let formalTys = map snd3 formalArgs in
  let argsok = all (\(formalTy,actualTy) -> 
                          if not (sameBaseTy formalTy actualTy) then
                            error $ "incompatible types\nfound "++showTy actualTy++ "\nrequired: "++showTy formalTy++"\n "++showImppTabbed exp 1
                          else True
                   ) (zip formalTys actualTys) in
  if (length formalTys /= length actualTys || 
      any (\(fTy,aTy) -> not (sameBaseTy fTy aTy)) (zip formalTys actualTys)) then
    error (fName ++ "(" ++ concatSepBy "," (map showTy formalTys) ++ ") cannot be applied to (" ++ concatSepBy "," (map showTy actualTys) ++ ")") 
  else return (newMds,LblMethCall lbl fName newArgs,retTy)

sTypeCheckExp prog exp@(While e1 eb) mn gamma = 
  sTypeCheckExp prog e1 mn gamma >>= \(newMd1,newE1,newTy1) ->
  if not (sameBaseTy newTy1 PrimBool{anno=Nothing}) then
    error $ "incompatible types in while test\nfound: "++showTy newTy1++"\nrequired: Bool\n "++showImppTabbed exp 1
  else
    sTypeCheckExp prog eb mn gamma >>= \(newMdEb,newEb,tyEb) ->
    if not (sameBaseTy tyEb PrimVoid{anno=Nothing}) then
      error $ "incompatible types in while body\nfound: "++showTy tyEb++"\nrequired: Void\n "++showImppTabbed exp 1
    else
      fresh >>= \freshNo -> let freshMname = "while"++freshNo in
      let retTy = PrimVoid{anno=Nothing} in
      let whileLits = nub (concatMap freeVars [e1,eb]) in
      let whileArgs = map (\lit -> ExpVar lit) whileLits in
      mapM (\arg -> case lookupVar arg gamma of
                                Nothing -> error $ "undefined variable "++arg++"\n "++ showImppTabbed exp 1
                                Just ty -> 
                                  freshTy ty >>= \annTy ->
                                  return (PassByRef,annTy,arg)
           ) whileLits >>= \argsAndTys ->
      let whileCall = LblMethCall Nothing freshMname whileArgs in
      let newWhileEb = ExpBlock [] (If False newE1 (Seq newEb whileCall) KVoid) in
      let newMD = MethDecl {methParams=((PassByRef,retTy,freshMname):argsAndTys),
                            methExternal=False,
                            methPost=(triple FormulaBogus),
                            methPres=[],
                            methUpsis=[],
                            methInv=FormulaBogus,
                            methOK=FormulaBogus,methERRs=[],
                            methNEVER=FormulaBogus,methMUSTs=[],methMAY=([],FormulaBogus),
                            methBody=newWhileEb} in
      return (newMD:newMd1++newMdEb,whileCall,PrimVoid{anno=Nothing})

sTypeCheckExp prog exp@(For e1 e2 e3 eb) mn gamma = 
  sTypeCheckExp prog e1 mn gamma >>= \(newMd1,newE1,newTy1) ->
  if not (sameBaseTy newTy1 PrimVoid{anno=Nothing}) then
    error $ ""
  else
    sTypeCheckExp prog e2 mn gamma >>= \(newMd2,newE2,newTy2) ->
    sTypeCheckExp prog e3 mn gamma >>= \(newMd3,newE3,newTy3) ->
    sTypeCheckExp prog eb mn gamma >>= \(newMdEb,newEb,newTyEb) ->
    fresh >>= \freshNo -> let freshMname = "for"++freshNo in
    let retTy = PrimVoid{anno=Nothing} in
    let forLits = nub (concatMap freeVars [e2,e3,eb]) in
    let forArgs = map (\lit -> ExpVar lit) forLits in
    mapM (\arg -> case lookupVar arg gamma of
                              Nothing -> error $ "undefined variable "++arg++"\n "++ showImppTabbed exp 1
                              Just ty -> 
                                freshTy ty >>= \annTy ->
                                return (PassByRef,annTy,arg)
         ) forLits >>= \argsAndTys ->
    let forCall = LblMethCall Nothing freshMname forArgs in
    let newForEb = ExpBlock [] (If False newE2 (Seq newEb (Seq newE3 forCall)) KVoid) in
    let newMD = MethDecl {methParams=((PassByRef,retTy,freshMname):argsAndTys),
                          methExternal=False,
                          methPost=(triple FormulaBogus),
                          methPres=[],
                          methUpsis=[],
                          methInv=FormulaBogus,
                          methOK=FormulaBogus,methERRs=[],
                          methNEVER=FormulaBogus,methMUSTs=[],methMAY=([],FormulaBogus),
                          methBody=newForEb} in
    return (newMD:newMd1++newMd2++newMd3++newMdEb,forCall,PrimVoid{anno=Nothing})

sTypeCheckExp prog exp@(ExpBlock varDecls eb) mn gamma =
  sTypeCheckVarDecls prog varDecls mn ([],[],gamma) >>= \(newMds,newVds,gamma1) ->
  sTypeCheckExp prog eb mn gamma1 >>= \(newMdsEb,newEb,tyEb) -> 
  return (newMds++newMdsEb,ExpBlock newVds newEb,tyEb)
  
sTypeCheckVarDecls::Prog -> [VarDecl] -> Lit -> ([MethDecl],[VarDecl],TypeEnv) -> FS ([MethDecl],[VarDecl],TypeEnv)
sTypeCheckVarDecls prog [] mn (partMds,partVds,gamma) = return (partMds,reverse partVds,gamma)
sTypeCheckVarDecls prog (vd:vds) mn (partMds,partVds,gamma) = 
  sTypeCheckVarDecl prog vd mn gamma >>= \(newMds,newVd,gamma1) ->
  sTypeCheckVarDecls prog vds mn (partMds++newMds,newVd:partVds,gamma1)
  
sTypeCheckVarDecl:: Prog -> VarDecl -> Lit -> TypeEnv -> FS ([MethDecl],VarDecl,TypeEnv)
sTypeCheckVarDecl prog vd@(VarDecl declTy lit e1) mn gamma =
  sTypeCheckExp prog e1 mn gamma >>= \(newMd1,newE1,newTy1) ->
  let gamma1 = extendTypeEnv gamma (lit,declTy) in
  if not (sameBaseTy declTy newTy1) then
    error $ "incompatible types\nfound: " ++ showTy newTy1 ++ "\nrequired: " ++ showTy declTy ++ "\n "++ showImppTabbed vd 1
  else
    return (newMd1,VarDecl declTy lit newE1,gamma1)

sTypeCheckVarDecl prog vd@(LblArrVarDecl lbl annTy indxs lit e1) mn gamma =
  sTypeCheckExp prog e1 mn gamma >>= \(newMd1,newE1,newTy1) ->
  mapM (\i -> sTypeCheckExp prog i mn gamma) indxs >>= \triple ->
  let (emptyMds,sameIndxs,indxTys) = unzip3 triple in
  if not $ all (\iTy -> sameBaseTy iTy PrimInt{anno=Nothing}) indxTys then
    error $ "incompatible types in array declaration - indexes must be integers\n "++showImppTabbed vd 1
  else
    let gamma1 = extendTypeEnv gamma (lit,annTy) in
    case annTy of
      ArrayType{elemType=eTy,indxTypes=iTys} ->
        if (length iTys /= length indxs) then
          error $ "incompatible no. of dimension is array declaration\n " ++ showImppTabbed vd 1
        else
          if not (sameBaseTy newTy1 eTy) then 
            error $ "incompatible types\nfound: " ++ showTy newTy1 ++ "\nrequired: " ++ showTy eTy  ++ "\n "++ showImppTabbed vd 1
          else
            return (newMd1,LblArrVarDecl lbl annTy indxs lit newE1,gamma1)
