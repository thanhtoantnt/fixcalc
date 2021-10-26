module ImpAST where
import Fresh(getFlags,FS(..))
import ImpConfig(isIndirectionIntArray,outputFile)
import MyPrelude
------------------------------------------
import Data.List(nub,(\\),union)
import System.IO.Unsafe(unsafePerformIO)

-------AST-------------------------
data Prog = Prog [String] [PrimDecl] [MethDecl]

-- | A declaration of a primitive is represented with (Arguments, Postcondition, PreconditionS, Runtime-testS).
data PrimDecl = PrimDecl {
  primParams:: [(PassBy,AnnoType,Lit)],
  primPost:: Formula,
  primPres:: [LabelledFormula],
  primTests:: [LabelledExp]
}
-- | A declaration of a method is represented with (Arguments, Body, Postcondition, Preconditions, Runtime-checkc, Invariant).
data MethDecl = MethDecl {
  methParams:: [(PassBy,AnnoType,Lit)],
  methBody  :: Exp,
  methExternal :: Bool,           -- ^Flag to indicate if the method is external (not called anywhere in the given program).
  methPost  :: [Formula],         -- ^Method postcondition
  methPres  :: [LabelledFormula], -- ^Method preconditions
  methUpsis :: [QLabel],          -- ^Labels for checks that need to be specialized. Computed in ImpTypeInfer and used in ImpSugar.
  methInv   :: Formula,           -- ^Transition invariant. Computed and used in ImpTypeInfer and ImpOutInfer.
  methOK    :: Formula,           -- ^OK outcome (equivalent to methPost). Computed and used in ImpOutInfer.
  methERRs  :: [LabelledFormula], -- ^ERR outcomes. Computed and used in ImpOutInfer.
  methNEVER :: Formula,           -- ^Never-bug condition (equivalent to methPres).
  methMUSTs :: [LabelledFormula], -- ^Must-bug conditions. 
  methMAY   :: LabelledFormula    -- ^May-bug condition.
}

data Callable = Prim PrimDecl
  | Meth MethDecl
data PassBy = PassByRef | PassByVal deriving (Show,Eq)

type LabelledFormula = (QLabel,Formula) -- ^precondition
type LabelledExp = (Label,Exp) -- ^runtime test
type QLabel = [Label]
type Label = String


-------Annotated Types-------------
data AnnoType = PrimInt {anno:: Maybe Anno}
  | PrimVoid {anno:: Maybe Anno}
  | PrimBool {anno:: Maybe Anno}
  | PrimFloat {anno:: Maybe Anno}
  | ArrayType {elemType::AnnoType, indxTypes::[AnnoType]}
  | TopType {anno:: Maybe Anno}

type Anno = String
-------Exp-------------------------
data Exp = KTrue
  | KFalse
  | KVoid
  | KIntNum Int
  | KFloatNum Float
  | ExpVar Lit
  | If Bool Exp Exp Exp -- ^the Bool flag indicates if the conditional is non-deterministic.
  | LblMethCall (Maybe Label) Lit [Exp]
  | AssignVar Lit Exp
  | Seq Exp Exp
  | ExpBlock [VarDecl] Exp
  | ExpError
  | ExpBogus  -- ^used in a variable declaration (where the initializer is missing).
  | While Exp Exp
  | For Exp Exp Exp Exp

data VarDecl = VarDecl AnnoType Lit Exp
  | LblArrVarDecl (Maybe Label) AnnoType [Exp] Lit Exp

type Lit=String

data Outcome = OK Formula | ERR Formula
getOKOutcome [OK f1, ERR f2] = f1
getOKOutcome _ = FormulaBogus
getERROutcome [OK f1, ERR f2] = f2
getERROutcome _ = FormulaBogus

-- Types for 3Contexts version
triple:: a -> [a]
triple = replicate 3
strong::[Formula] -> Formula
strong = head
weak::[Formula] -> Formula
weak (x:xs) = head xs
cond::[Formula] -> Formula
cond (x1:x2:xs) = head xs
-- Types for 3Contexts version

-------Formula---------------------
data RecPost = RecPost Lit Formula ([QSizeVar],[QSizeVar],[QSizeVar])
-- ^This is the type that corresponds to a constraint abstraction.
-- Its arguments are: name body (inputs,outputs,imperByValue).
-- The meaning: (fExists (primeTheseSizeVars imperByValue) body) && noChange(imperByValue).

type Relation = ([QSizeVar],[QSizeVar],Formula)

type QFormula = ([QSizeVar],Formula)

data Formula = 
    And [Formula]
  | Or [Formula]
  | Not Formula
  | Exists [QSizeVar] Formula
  | GEq [Update]
  | EqK [Update] --EqK instead of Eq: be careful to check some Updates to be positive, others to be negative
  | AppRecPost Lit [QSizeVar]
-- deprecated Constructors: do not use them anymore
  | Forall [QSizeVar] Formula
  | Union [Formula]
  | FormulaBogus
  deriving Eq

data Update = Const Int
  | Coef QSizeVar Int
  deriving Eq

-------SizeVar---------------------
type QSizeVar = (SizeVar,PorU) 
data SizeVar = SizeVar Anno
  | ArrSizeVar Anno MorM
  deriving (Eq,Show)
data PorU= Primed
  | Unprimed
  | Recursive
  deriving (Eq,Show)
data MorM = Min | Max 
  deriving (Eq,Show)

showTest3 = \s-> show (Test3 s)
  
-------Functions-----------
primName:: PrimDecl -> Lit
primName p = thd3 (head (primParams p))

methName:: MethDecl -> Lit 
methName m = thd3 (head (methParams m))

updateMethDecl (Prog incls prims oldMeths) newm = 
  let newMeths = map (\oldm -> 
                          if methName oldm == methName newm then newm 
                          else oldm
                      ) oldMeths in (Prog incls prims newMeths)

-- list of [MethOrPrim] is assumed not to contain duplicates (same criteria of equality)
findCallable:: Lit -> [Callable] -> Maybe Callable
findCallable lit [] = Nothing

findCallable fName (c:cs) =
  case c of
    Meth m ->
      if methName m == fName then 
        Just c 
      else findCallable fName cs
    Prim p ->
      if primName p == fName then 
        Just c 
      else findCallable fName cs

-- requires:: mn method should be defined in Prog (findMethod cannot fail)
findMethod:: Lit -> Prog -> MethDecl
findMethod mn (Prog _ prims meths) = findMethod1 mn meths where
  findMethod1 mn (m:ms) | methName m == mn = m
  findMethod1 mn (m:ms) | methName m /= mn = findMethod1 mn ms

----Functions on types-------------
isPrimitiveType:: AnnoType -> Bool
isPrimitiveType PrimBool{} = True
isPrimitiveType PrimFloat{} = True
isPrimitiveType PrimInt{} = True
isPrimitiveType PrimVoid{} = True
isPrimitiveType TopType{} = error "isPrimitiveType: argument must not be TopType"
isPrimitiveType _ = False

isIndirectionArrTy:: AnnoType -> FS Bool
isIndirectionArrTy ArrayType{elemType=PrimInt{}} = 
  getFlags >>= \flags ->
  return (isIndirectionIntArray flags)
isIndirectionArrTy _ = return False
-----------------------------------
-- |Given e, this function returns the free variables used in e. Be sure to remove duplicates before using this function.
freeVars:: Exp -> [Lit]
freeVars (ExpVar lit) = [lit]
freeVars (AssignVar lit e) = lit:freeVars e
freeVars (If _ e1 e2 e3) = concatMap freeVars [e1,e2,e3]
freeVars (LblMethCall lbl id es) = concatMap freeVars es
freeVars (Seq e1 e2) = freeVars e1 ++ freeVars e2
freeVars (ExpBlock vds eb) = concatMap freeVarsVarDecl vds++freeVars eb
freeVars (While e1 eb) = freeVars e1 ++ freeVars eb
freeVars (For e1 e2 e3 eb) = concatMap freeVars [e1,e2,e3,eb]
freeVars _ = []

freeVarsVarDecl:: VarDecl -> [Lit]
freeVarsVarDecl (VarDecl primTy lit e1) = lit:freeVars e1
freeVarsVarDecl (LblArrVarDecl lbl arrTy indxs lit e1) = lit:concatMap freeVars (e1:indxs)


-------Canonical Formula-----------
fTrue::Formula
fTrue = EqK [Const 0]

fFalse::Formula
fFalse = EqK [Const 1]

fAnd:: [Formula] -> Formula
fAnd fs = 
  if (null fs) then fTrue -- error $ "And formula without any clauses -- should I return True?"
  else if (singleton fs) then head fs else And fs

fOr:: [Formula] -> Formula
fOr fs = 
  if (null fs) then fFalse -- error $ "Or formula without any clauses - should I return False?"
  else if (singleton fs) then head fs else Or fs

fNot:: Formula -> Formula
fNot f = Not f

fExists:: [QSizeVar] -> Formula -> Formula
fExists vs f = if (null vs) then f 
  else (Exists vs f)

fForall:: [QSizeVar] -> Formula -> Formula
fForall vs f = if (null vs) then f else (Forall vs f)  

fGT:: [Update] -> Formula
fGT ups = GEq (Const (-1):ups)

-- |Counts the number of AppRecPost in a formula.
countAppRecPost:: Formula -> Int
countAppRecPost formula = case formula of
  And fs -> sum (map (\f -> countAppRecPost f) fs)
  Or fs -> sum (map (\f -> countAppRecPost f) fs)
  GEq us -> 0
  EqK us -> 0
  Exists qsvs f -> countAppRecPost f
  Forall qsvs f -> countAppRecPost f
  Not f -> countAppRecPost f
  AppRecPost _ _ -> 1
  _ -> error ("countAppRecPost: unexpected argument: "++show formula)
    
-------Selectors from Formulae------------
-- |Extracts size variables from a formula without keeping DUPLICATES
fqsv:: Formula -> [QSizeVar]
fqsv f = nub $ case f of 
  And formulae -> concatMap (\f -> fqsv f) formulae
  Or formulae -> concatMap (\f -> fqsv f) formulae
  Not formula -> fqsv formula
  Exists otherSVs formula -> 
    let inside = (fqsv formula) in 
      inside \\ otherSVs
  Forall otherSVs formula -> 
    let inside = (fqsv formula) in
      inside \\ otherSVs
  GEq ups -> fqsvU ups 
  EqK ups -> fqsvU ups 
  AppRecPost lit insouts -> insouts
  FormulaBogus -> []
  _ -> error ("fqsv: unexpected argument: " ++ show f)

fqsvU:: [Update] -> [QSizeVar]
fqsvU [] = []
fqsvU (up:ups) = 
  let rest=fqsvU ups in 
    case up of
      Const int -> rest
      Coef qsv int -> qsv:rest  -- << Diferent from sizeVarsFromUpdates

-------Show Prog-------------------
--Problematic !!
-- multi-dimensional arrays as function arguments: all sizes (but the last) must be filled - with what??
-- test in conditional, init value in varDecl, arguments of function and rhs of assign must be proper expressions (no varDecl, if, seq)
      
--showC - C code (as close as possible)
class ShowC a where
  showC:: a -> String
  showC a = showCTabbedRet a (False,"") 0
  -- showCTabbed with "return" or without (inserting only ;)
  showCTabbedRet:: a -> (Bool,String) -> Int -> String
  showCTabbedRet a (b,pre) cnt = showC a

tabs:: Int -> String
tabs x = replicate x ' '

instance ShowC Prog where
  showC (Prog inclFilenames prims meths) = showPreludeC inclFilenames ++ concatMap showC meths
    where
    showPreludeC:: [String] -> String
    showPreludeC _ = "#include \"Primitives.h\"\n\n"

instance ShowC MethDecl where
  showC m =
    let ((_,t,fname):args) = methParams m in
    let strArgs = concatArgsC args in
      showC t ++ " " ++ fname ++ "(" ++ strArgs ++ ")" ++ showExpAsBlockC (methBody m) (True,"return ") 1 ++ "\n\n"
    where
    concatArgsC:: [(PassBy,AnnoType,Lit)] -> String
    concatArgsC [] = ""
    concatArgsC [(_,ty,arg)] = showC ty ++ " " ++ arg
    concatArgsC ((_,ty,arg):strs) = showC ty ++ " " ++ arg ++ "," ++ concatArgsC strs

instance ShowC AnnoType where
  showC ty = 
    case ty of
      PrimBool{} -> "bool"
      PrimFloat{} -> "float"
      PrimInt{} -> "int"
      PrimVoid{} -> "void"
      ArrayType{elemType=PrimInt{}} -> 
            case length (indxTypes ty) of
              1 -> "arr"
              2 -> "arr2"
      ArrayType{elemType=PrimFloat{}} -> 
            case length (indxTypes ty) of
              1 -> "arrF"
              2 -> "arrF2"
      ArrayType{} -> "no_C_type" --no C counterpart yet

instance ShowC Exp where
  showCTabbedRet e (addRet@(b,pre)) cnt = 
    case e of
      KTrue -> if b then pre ++ "TRUE" else "TRUE"
      KFalse -> if b then pre ++ "FALSE" else "FALSE"
      KIntNum i -> if b then pre ++ show i else show i
      KFloatNum f -> if b then pre ++ show f else show f
      KVoid -> if b then pre else ";"
      ExpError -> "printf(\"ABC failed.\\n\");exit(1)"
      ExpBogus -> "NOT INITIALIZED"
      ExpVar id -> if b then pre ++ id else id
      AssignVar id exp -> id ++ "=" ++ showCTabbedRet exp addRet cnt
      LblMethCall lbl id args -> 
        let call = id ++ "(" ++ concatSepBy "," (map (\arg -> showCTabbedRet arg (False,"") cnt) args) ++ ")" in
        if b then pre++call else call
      ExpBlock _ _ -> showExpAsBlockC e addRet cnt
      If _ test exp1 exp2 -> 
        "if (" ++ showCTabbedRet test (False,"") cnt ++ ")" ++ showExpAsBlockC exp1 addRet (cnt+1) ++
        " else" ++ showExpAsBlockC exp2 addRet (cnt+1)
      Seq exp1 exp2 -> showCTabbedRet exp1 (False,"") cnt ++ ";\n" ++ tabs cnt ++ showCTabbedRet exp2 addRet cnt
      
showExpAsBlockC:: Exp -> (Bool,String) -> Int -> String
showExpAsBlockC e addRet cnt = 
  case e of
    ExpBlock varDecls eb -> " {\n" ++ tabs cnt ++ concatMap (\vd -> showCTabbedRet vd (False,"") cnt) varDecls ++ 
      showBlockAsExpC eb addRet (cnt+1) ++ tabs cnt ++ "}"
    _ -> " {\n" ++ tabs cnt ++ showCTabbedRet e addRet cnt ++ ";\n" ++ tabs cnt ++ "}"

showBlockAsExpC:: Exp -> (Bool,String) -> Int -> String
showBlockAsExpC e addRet cnt =
  case e of
    ExpBlock varDecls eb -> concatMap (\vd -> showCTabbedRet vd (False,"") cnt) varDecls ++ showBlockAsExpC eb addRet cnt
    _ -> showCTabbedRet e addRet cnt ++ ";\n"

instance ShowC VarDecl where
  showCTabbedRet vd addRet cnt = 
    case vd of
      VarDecl ty lit exp -> 
        case exp of
          If _ e1 e2 e3 -> 
            showC ty ++ " " ++ lit ++ "; " ++
            showCTabbedRet exp (True,lit++" = ") (cnt+1) ++ ";\n" ++ tabs cnt          
          _ -> 
            showC ty ++ " " ++ lit ++ " = " ++ showCTabbedRet exp addRet (cnt+1) ++ ";\n" ++ tabs cnt
      LblArrVarDecl (Just lbl) ty@(ArrayType {elemType=eType,indxTypes=iTypes}) indxs lit exp -> 
        case exp of
          If _ e1 e2 e3 -> error $ "array initialized with if: the C code will not compile. Better desugar Imp code."
          _ ->
            let dim1 = showCTabbedRet (indxs!!0) (False,"") (cnt+1) in
            let (call,dims) =  case (eType,length iTypes) of {
                    (PrimInt{},1) -> ("_initArr",dim1);
                    (PrimFloat{},1) -> ("_initArrF",dim1);
                    (PrimInt{},2) -> ("_initArr2",dim1++","++showCTabbedRet (indxs!!1) addRet (cnt+1));
                    (PrimFloat{},2) -> ("_initArrF2",dim1++","++showCTabbedRet (indxs!!1) addRet (cnt+1));
                    (PrimVoid{},1) -> ("_initArrV",dim1);
                    (PrimBool{},1) -> ("_initArrB",dim1)} in
              showC ty ++ " " ++ lit ++ "; " ++ call ++ "(&" ++ 
              showCTabbedRet (ExpVar lit) addRet (cnt+1) ++ "," ++ 
              showCTabbedRet exp addRet (cnt+1) ++ "," ++ 
              dims ++ ");\n" ++ tabs cnt --only one or two dimensions!!


--showImpp - with type annotations, labels, post and pres (Imp+ language)
class ShowImpp a where
  showImpp:: a -> String
  showImpp a = showImppTabbed a 0
  showImppTabbed:: a -> Int -> String
  showImppTabbed a cnt = showImpp a

-- (showImpp Formula) is related to (show Formula)
-- (show Formula) is used in log and in error-messages.
-- (showImpp Formula) is used for pretty-printing to *.imp files. 
-- Otherwise *.imp files are not type-checkable, since type-checking assumes f_0 is a fresh variable.
suffixVar = 'f' --'f' is added at the end of size variables in types and in formaule: (f_0 -> f_0f) (f_1f -> f_1ff)

instance ShowImpp Prog where
  showImpp (Prog inclFilenames prims meths) = 
    showPreludeImpp inclFilenames ++ concatMap showImpp meths
    where
    showPreludeImpp:: [String] -> String
    showPreludeImpp [] = ""
    showPreludeImpp [inclFilename] = "#include \"" ++ inclFilename ++ "\"\n\n"
    showPreludeImpp (i:inclFilenames) = showPreludeImpp [i] ++ showPreludeImpp inclFilenames

instance ShowImpp MethDecl where
  showImpp m = 
    let ((passby,t,fname):args) = methParams m in
    let passbyStr = if passby==PassByRef then "ref " else "" in
    let strArgs = concatArgs args in
    "{-\nOK:="++showSet (methOK m) ++ "\n" ++
    "ERRs:={" ++ showImpp (methERRs m)++"}\n"++
-- printing NEVER/MUST/MAY is expensive. For benchmark evaluation is disabled:
    "NEVER_BUG:=" ++showSet (methNEVER m) ++ "\n" ++
    "MUST_BUGs:={"++showImpp (methMUSTs m) ++ "}\n" ++
    "MAY_BUG:="   ++showImpp [methMAY m] ++ "\n-}\n" ++
    passbyStr ++ showImpp t ++ " " ++ fname ++ "(" ++ strArgs ++ ")" ++ 
    "\n  where\n  (" ++ showImpp (strong $ methPost m) ++ "),\n  {" ++ showImpp (methPres m) ++ "}" ++
--    ",\n  {" ++ showImpp (methUpsis m) ++ "},\n  (" ++ showImpp (methInv m) ++ ")," ++
    "\n{" ++ showImppTabbed (methBody m) 1 ++ "}\n\n"

instance ShowImpp PrimDecl where
  showImpp p = 
    let ((_,t,fname):args) = primParams p in
    let strArgs = concatArgs args in
      showImpp t ++ " " ++ fname ++ "(" ++ strArgs ++ ")\n  where\n  (" ++ 
      showImpp (primPost p) ++ "),\n  {" ++ 
      showImpp (primPres p) ++ "},\n  {" ++ showTestsImpp (primTests p) ++ 
      "},\n  (" ++ show fTrue ++ "),\n\n"
    where
    showTestsImpp:: [LabelledExp] -> String
    showTestsImpp [] = ""
    showTestsImpp ((lbl,exp):rest) = 
      lbl ++ ":" ++ showImppTabbed exp 1 ++ "," ++ showTestsImpp rest

concatArgs:: [(PassBy,AnnoType,Lit)] -> String
concatArgs [] = ""
concatArgs [(passby,ty,arg)] = 
  let passbyStr = if passby==PassByRef then "ref " else "" in
    passbyStr ++ showImpp ty ++ " " ++ arg
concatArgs ((passby,ty,arg):strs) = 
  let passbyStr = if passby==PassByRef then "ref " else "" in
    passbyStr ++ showImpp ty ++ " " ++ arg ++ "," ++ concatArgs strs

instance ShowImpp AnnoType where
  showImpp ty = 
    case ty of
      PrimBool{anno=Just a} -> "Bool" ++ "<" ++ a++[suffixVar] ++ ">"
      PrimBool{anno=Nothing} -> "Bool"
      PrimFloat{anno=Just a} -> "Float" ++ "<" ++ a++[suffixVar] ++ ">"
      PrimFloat{anno=Nothing} -> "Float"
      PrimInt{anno=Just a} -> "Int" ++ "<" ++ a++[suffixVar] ++ ">"
      PrimInt{anno=Nothing} -> "Int"
      PrimVoid{anno=Just a} -> "Void" ++ "<" ++ a++[suffixVar] ++ ">"
      PrimVoid{anno=Nothing} -> "Void"
      ArrayType{} -> 
        showImpp (elemType ty) ++ 
        "[" ++ concatSepBy "," (map showImpp (indxTypes ty)) ++ "]"
    
-- shows AnnoType without annotations: used in simple-type-checking
showTy:: AnnoType -> String
showTy ty =
  case ty of
    PrimBool{} -> "Bool"
    PrimFloat{} -> "Float"
    PrimInt{} -> "Int"
    PrimVoid{} -> "Void"
    ArrayType{elemType=eTy,indxTypes=iTys} -> 
      showTy eTy ++ "[" ++ (concatSepBy "," (map showTy iTys)) ++ "]"

instance ShowImpp [LabelledFormula] where
  showImpp [] = ""
  showImpp [(lbl,pre)] = showImpp lbl ++ ":(" ++ showImpp pre ++ ")"
  showImpp (pre:pres) = showImpp [pre] ++ "," ++ showImpp pres

instance ShowImpp [QLabel] where
  showImpp [] = ""
  showImpp [u] = showImpp u
  showImpp (u:upsis) = showImpp [u] ++ "," ++ showImpp upsis

instance ShowImpp QLabel where
  showImpp [] = ""
  showImpp [l] = l
  showImpp (l:ls) = l ++ "." ++ showImpp ls

instance ShowImpp Exp where
  showImppTabbed e cnt =
    case e of
      KTrue -> "True"
      KFalse -> "False"
      KIntNum i -> show i
      KFloatNum f -> show f
      KVoid -> "Void"
      ExpError -> "error"
      ExpBogus -> "NOT INITIALIZED"
      ExpVar id -> id
      AssignVar id exp -> id ++ ":=" ++ showImppTabbed exp cnt
      LblMethCall maybeLbl id args -> let lbl = case maybeLbl of {Just lbl->lbl;Nothing->"NO_LBL"} in
        lbl ++ ":" ++
        id ++ "(" ++ concatSepBy "," (map (\arg -> showImppTabbed arg cnt) args) ++ ")"
      ExpBlock varDecls eb -> "{\n" ++ tabs cnt ++ concatMap (\vd -> showImppTabbed vd cnt) varDecls ++ 
        showImppTabbed eb (cnt+1) ++ "\n" ++ tabs cnt ++ "}"
      If _ test exp1 exp2 -> 
        "if " ++ showImppTabbed test cnt ++ "\n" ++ tabs cnt ++ "then { " ++ 
          showImppTabbed exp1 (cnt+1) ++ "\n"++tabs cnt++"} else { " ++ showImppTabbed exp2 (cnt+1) ++ " }"
      Seq exp1 exp2 -> showImppTabbed exp1 cnt ++ ";\n" ++ tabs cnt ++ showImppTabbed exp2 cnt
      While exp1 eb -> "while " ++ showImppTabbed exp1 cnt ++ "\n" ++ tabs cnt ++ showImppTabbed eb (cnt+1)
      For exp1 exp2 exp3 eb -> "for (" ++ showImppTabbed exp1 cnt ++ "; " ++ showImppTabbed exp2 cnt ++ "; "
        ++ showImppTabbed exp3 cnt ++ ")\n" ++ tabs cnt ++ showImppTabbed eb (cnt+1)

instance ShowImpp VarDecl where
  showImppTabbed vd cnt = 
    case vd of
      VarDecl ty lit e -> showImpp ty ++ " " ++ lit ++ " := " ++ showImppTabbed e (cnt+1) ++ ";\n" ++ tabs cnt
      LblArrVarDecl maybeLbl ty indxs lit exp -> 
        let lbl = case maybeLbl of {Nothing -> "";Just lbl -> lbl++":"} in
        lbl ++ showImpp ty ++ "[" ++ concatSepBy "," (map showImpp indxs) ++ "] " ++ lit ++ " := " ++ showImppTabbed exp (cnt+1) ++ ";\n" ++ tabs cnt

instance ShowImpp [Outcome] where
  showImpp [ok,err] = "{" ++ showImpp ok ++ ", " ++ showImpp err ++ "}"
instance ShowImpp Outcome where
  showImpp (OK f) = "OK: "++showImpp f
  showImpp (ERR f) = "ERR: "++showImpp f

instance ShowImpp Formula where
  showImpp (And c) = 
        let show_vec = (\fs -> 
              case fs of
                [] -> show "And--void--"
                [c] -> showImpp c
                (c:cs) -> showImpp c ++ " && " ++ show_vec cs)
        in "(" ++ show_vec c ++ ")"
  showImpp (Or c) =
        let show_vec = (\fs -> 
              case fs of
                [] -> show "Or--void--"
                [c] -> showImpp c
                (c:cs) -> showImpp c ++ " || " ++ show_vec cs)
        in "(" ++ show_vec c ++ ")"
  showImpp (Not c) = "(! " ++ showImpp c ++ ")"
  showImpp (Exists qsvs f) = "exists (" ++ concatSepBy "," (map showImpp qsvs) ++ " : " ++ showImpp f ++ ")"
  showImpp (Forall qsvs f) = error $ "forall should not appear in Impp form" ++ show f
  showImpp (GEq us) = 
    if (length us == 0) then "GEq--void--"
    else
      let lhs_terms = filter (\u -> case u of {Const i -> i>0; Coef qsv i -> i>=0}) us in
      let rhs_terms = filter (\u -> case u of {Const i -> i<0; Coef qsv i -> i<0}) us in
      let rhs_terms_pos = map (\u -> case u of {Const i -> Const (-i); Coef qsv i -> Coef qsv (-i)}) rhs_terms in
      let lhs = if (length lhs_terms == 0) then "0" else concatSepBy " + " (map showImpp lhs_terms) in
      let rhs = if (length rhs_terms_pos == 0) then "0" else concatSepBy " + " (map showImpp rhs_terms_pos) in
      lhs ++ " >= " ++ rhs
  showImpp (EqK us) = 
    if (length us == 0) then "EqK--void--"
    else
      let lhs_terms = filter (\u -> case u of {Const i -> i>0; Coef qsv i -> i>=0}) us in
      let rhs_terms = filter (\u -> case u of {Const i -> i<0; Coef qsv i -> i<0}) us in
      let rhs_terms_pos = map (\u -> case u of {Const i -> Const (-i); Coef qsv i -> Coef qsv (-i)}) rhs_terms in
      let lhs = if (length lhs_terms == 0) then "0" else concatSepBy " + " (map showImpp lhs_terms) in
      let rhs = if (length rhs_terms_pos == 0) then "0" else concatSepBy " + " (map showImpp rhs_terms_pos) in
      lhs ++ " = " ++ rhs
  showImpp f@(AppRecPost lit insouts) = error $ "RecPost should not appear in Impp form" ++ show f
  showImpp (FormulaBogus) = " NOT INITIALIZED "

instance ShowImpp Update where
  showImpp (Const i) = show i
  showImpp (Coef qsv i) =
      let (bef,aft) = if (i==1) then ("","") else if (i==(-1)) then ("(-",")") else ((show i) ++ "*","") in
      bef ++ showImpp qsv ++ aft

instance ShowImpp QSizeVar where
  showImpp (sv,pORu) =
    let pu = if pORu == Primed then "PRM" else if pORu == Recursive then "REC" else "" in
    let str = case sv of
                  SizeVar ann -> ann
                  ArrSizeVar ann Min -> "DTm" ++ ann
                  ArrSizeVar ann Max -> "DTM" ++ ann
    in (pu ++ str ++ [suffixVar])

getUpsisFromProg (Prog _ _ meths) = 
    let (upsis,nos) = unzip (map getUpsisFromMeth meths) in
      (concat upsis,sum nos)
    where
    getUpsisFromMeth:: MethDecl -> (String,Int)
    getUpsisFromMeth m = 
      let upsis = methUpsis m in
        if length upsis == 0 then ("",0)
        else 
          let upsis = methUpsis m in
          let no = length upsis in
            ("{" ++ (methName m ++ ": " ++ concatSepBy "," (map showImpp upsis)) ++ "}",no)
            
showImppMethForChecking:: MethDecl -> String
showImppMethForChecking m = 
  let ((passby,t,fname):args) = methParams m in
  let passbyStr = if passby==PassByRef then "ref " else "" in
  let strArgs = concatArgs args in
        passbyStr ++ showImpp t ++ " " ++ fname ++ "(" ++ strArgs ++ ")\n  where\n  (" ++ 
        "},\n{" ++ showImppTabbed (methBody m) 1 ++ "}\n\n"

-------Show Formula----------------
instance Show RecPost where
  show (RecPost fname formula (ins,outs,byVal)) = 
    fname ++ ":={[" ++ concatSepBy "," (map (\ s-> show (Test3 s))ins) ++ "] -> [" ++ concatSepBy "," (map (showTest3) outs) ++ "] -> [" ++ concatSepBy "," (map (showTest3) byVal) ++ "]: (" ++ show formula ++ ")};\n"
    
instance Show Formula where
    show (And c) = 
      let show_vec = (\fs -> 
            case fs of
              [] -> show "And--void--"
              [c] -> show c
              (c:cs) -> show c ++ " && " ++ show_vec cs)
      in "(" ++ show_vec c ++ ")"
    show (Or c) =
      let show_vec = (\fs -> 
            case fs of
              [] -> show "Or--void--"
              [c] -> show c
              (c:cs) -> show c ++ " || " ++ show_vec cs)
      in "(" ++ show_vec c ++ ")"
    show (Not c) = "(! " ++ show c ++ ")"
    show (Exists qsvs f) = "exists (" ++ concatSepBy "," (map (showTest3) qsvs) ++ " : " ++ show f ++ ")"
    show (Forall qsvs f) = "forall (" ++ concatSepBy "," (map (showTest3) qsvs) ++ " : " ++ show f ++ ")"
    show (GEq us) = 
      if (length us == 0) then "GEq--void--"
      else
        let lhs_terms = filter (\u -> case u of {Const i -> i>0; Coef qsv i -> i>=0}) us in
        let rhs_terms = filter (\u -> case u of {Const i -> i<0; Coef qsv i -> i<0}) us in
        let rhs_terms_pos = map (\u -> case u of {Const i -> Const (-i); Coef qsv i -> Coef qsv (-i)}) rhs_terms in
        let lhs = if (length lhs_terms == 0) then "0" else concatSepBy " + " (map show lhs_terms) in
        let rhs = if (length rhs_terms_pos == 0) then "0" else concatSepBy " + " (map show rhs_terms_pos) in
        lhs ++ " >= " ++ rhs
    show (EqK us) = 
      if (length us == 0) then "EqK--void--"
      else
        let lhs_terms = filter (\u -> case u of {Const i -> i>0; Coef qsv i -> i>=0}) us in
        let rhs_terms = filter (\u -> case u of {Const i -> i<0; Coef qsv i -> i<0}) us in
        let rhs_terms_pos = map (\u -> case u of {Const i -> Const (-i); Coef qsv i -> Coef qsv (-i)}) rhs_terms in
        let lhs = if (length lhs_terms == 0) then "0" else concatSepBy " + " (map show lhs_terms) in
        let rhs = if (length rhs_terms_pos == 0) then "0" else concatSepBy " + " (map show rhs_terms_pos) in
        lhs ++ " = " ++ rhs
    show (AppRecPost lit insouts) = lit ++ "(" ++ concatSepBy "," (map (showTest3) insouts) ++ ")"
    show (FormulaBogus) = " NOT INITIALIZED "

instance Show Update where
    show (Const i) = show i
    show (Coef qsv i) = 
      -- added a "*" so that 2n is displayed as 2*n
      let (bef,aft) = if (i==1) then ("","") else if (i==(-1)) then ("(-",")") else ((show i) ++ "*","") in
      bef ++ (show (Test3 qsv)) ++ aft

newtype Test3 = Test3 QSizeVar
instance Show Test3 where
  show (Test3(sv,pORu)) = 
    let pu = if pORu == Primed then "PRM" else if pORu == Recursive then "REC" else "" in
    let str = case sv of
                  SizeVar ann -> ann
                  ArrSizeVar ann Min -> "DTm" ++ ann
                  ArrSizeVar ann Max -> "DTM" ++ ann
    in (pu ++ str)

---- 2 different textual forms for QSizeVar: one from input file and the other from Omega (previously converted with show:: QSizeVar -> String)
-- from input: s,s',  s^,  s.min,s.max,s.min', s.max', s.min^, s.max^
-- from Omega: s,PRMs,RECs,DTms, DTMs, PRMDTms,PRMDTMs,RECDTms,RECDTMs
stringToQsv:: String -> QSizeVar
stringToQsv s = 
  case (take 3 s) of
    "PRM" -> case (take 3 (drop 3 s)) of
      "DTm" -> if (drop 6 s == "") then error "PRMDTm error" else (ArrSizeVar (drop 6 s) Min,Primed)
      "DTM" -> if (drop 6 s == "") then error "PRMDTM error" else (ArrSizeVar (drop 6 s) Max,Primed)
      _ -> if (drop 3 s == "") then error "PRM error" else (SizeVar (drop 3 s),Primed)
    "DTm" -> if (drop 3 s == "") then error "DTm error" else (ArrSizeVar (drop 3 s) Min,Unprimed)
    "DTM" -> if (drop 3 s == "") then error "DTM error" else (ArrSizeVar (drop 3 s) Max,Unprimed)
-- Added: 25.03.2006
    "REC" -> case (take 3 (drop 3 s)) of
      "DTm" -> if (drop 6 s == "") then error "RECDTm error" else (ArrSizeVar (drop 6 s) Min,Recursive)
      "DTM" -> if (drop 6 s == "") then error "RECDTM error" else (ArrSizeVar (drop 6 s) Max,Recursive)
      _ -> if (drop 3 s == "") then error "REC error" else (SizeVar (drop 3 s),Recursive)
    _ -> (SizeVar s,Unprimed)

-- showSet and showRelation are used in the log (a.omega)
-- mimic the input that Omega accepts
newtype Test12  = Test12 ([String],Formula)
showSet:: Formula -> String
showSet f = let qsv = fqsv f in show (Test12 (map (showTest3) qsv,f))

newtype Test13  = Test13 ([String],[String],Formula)

showRelation:: Relation -> String
showRelation (from,to,f) = show (Test13(map (\s-> show (Test3 s)) from,map (\s-> show (Test3 s)) to,f))

instance Show (Test12) where
  show (Test12(vars,f)) = "{[" ++ concatSepBy "," vars ++ "]:" ++ show f ++ "};"

instance Show (Test13) where
  show (Test13(vars,vars',f)) =  "{[" ++ concatSepBy "," vars ++ "] -> [" ++ concatSepBy "," vars' ++ "]:" ++ show f ++ "};"

printProgImpt:: Prog -> FS ()
printProgImpt prog = 
  getFlags >>= \flags ->
  let outFile = outputFile flags ++ ".impt" in
  (unsafePerformIO $ writeFile outFile (showImpp prog)) `seq` return ()

printProgImpi:: Prog -> FS ()
printProgImpi prog = 
  getFlags >>= \flags ->
  let outFile = outputFile flags ++ ".impi" in
  (unsafePerformIO $ writeFile outFile (showImpp prog)) `seq` return ()

printProgC:: Prog -> FS()
printProgC prog = 
  getFlags >>= \flags ->
  let outFile = outputFile flags ++ ".c" in
  (unsafePerformIO $ writeFile outFile (showC prog)) `seq` return ()

printProgCAll:: Prog -> FS()
printProgCAll prog = 
  getFlags >>= \flags ->
  let outFile = outputFile flags ++ ".all.c" in
  (unsafePerformIO $ writeFile outFile (showC prog)) `seq` return ()

saturateFS :: Formula -> FS Formula
saturateFS f = return (saturate f)

saturate :: Formula -> Formula
saturate f =
  case f of
    And fs ->
      And (liftAnd (map saturate fs))
    Or fs -> 
      Or (map saturate fs)
    Exists vars ff -> f 
    GEq us -> f
    -- below will remove 0=0
    EqK [Const _] -> And []
    EqK us -> And [f,GEq us,GEq (map revSign us)]
    _ -> f

liftAnd :: [Formula] -> [Formula]
liftAnd ls =
  let ans = helper ls in
  case ans of
    [] -> [EqK [Const 0]]
    _ -> ans
  where
    helper [] = []
    helper ((And l):ls) = (helper l)++(helper ls)
    helper (x:ls) = x:(helper ls)

revSign :: Update -> Update
revSign u =
  case u of
    Const i -> Const (-i) 
    Coef qsv i -> Coef qsv (-i)

