{
module ImpParser where
import ImpAST
import ImpLexer(runP,P(..),Tk(..),lexer,getLineNum,getInput)
import MyPrelude
}

%monad {P}
%lexer {lexer} {TkEOF}
%tokentype {Tk}
%token
	lit	{TkAlphaNum $$}
	intNum	{TkIntNum $$}
	floatNum {TkFloatNum $$}
  stringLit {TkString $$}
	true	{TkTrue}
	false	{TkFalse}
	bogus {TkBogus}
	void	{TkKwVoid}
	int		{TkKwInt}
	bool	{TkKwBool}
	float	{TkKwFloat}
	if		{TkKwIf}
	ifnd	{TkKwIfND}
	then	{TkKwThen}
	else	{TkKwElse}
  while {TkKwWhile}
  for   {TkKwFor}
  do    {TkKwDo}
  ref   {TkKwRef}
	'+'		{TkPlus}
	'-'		{TkMinus}
	'*'		{TkMul}
	'/'		{TkDiv}
	'('		{TkLBr}
	')'		{TkRBr}
	';'		{TkSemiColon}
	':='	{TkAssign}
	'['		{TkLSqBr}
	']'		{TkRSqBr}
	'{'		{TkLAcc}
	'}'		{TkRAcc}
	','		{TkComma}
	'='   {TkEq}
	'<'   {TkLT}
	'>'   {TkGT}
	'>='  {TkGTE}
	'<='  {TkLTE}	
	'&&'   {TkAnd}
	'||'  {TkOr}
	where {TkWhere}
	':'   {TkColon}
	'.'   {TkDot}
	exists{TkExists}
	forall{TkForall}
	prime {TkPrime}
	rec   {TkRec}
	'error' {TkError}
  '#'   {TkHash}
  'include' {TkKwInclude}
--	'!'   {TkNot}
--	'<>'  {TkNEq}

%left '||'
%left '&&'
%nonassoc IF    -- lower than ; > + because at ; we must shift: if x then 1 else 2;3 
%nonassoc '>' '<' '>=' '<=' '='
%left ';'
%right ASSIGN
%left ','         -- , must be higher than < but lower than + because 1+x<y,z+2
%left '+' '-'
%left '*' '/'
%left '!'
%left NEG

{-
  -> QLabel is (and it should be) mandatory - LabelledFormula, LabelledExp
  -> mandatory separator between PrimDecl and MethDecl is %%
  -> assignments use :=, while = is used as relop (in both expressions and formulae).
    I cannot use = for formula equality and expression assignment, because they have different precedences
  -> I use Void both for type and constant. 
    If I would have used () instead of the constant Void, functions without args cannot be recognized unless I force a space between functions brackets.
  -> any formula that appears as a postcondition or precondition must be enclosed in brackets
    reason is that preconditions are separated by , which is also allowed inside formulae
  -> almost all operators bind tighter then IF. Use brackets around IF
    if x then 3 else 4; waits for another expression to finish the sequence. If this is at the end of a block - parsing fails.
-}

%name parseProg Prog
%name parseLPrimDecl LPrimDecl
%%

Prog::{IO Prog}
Prog: LInclude LMethDecl	{
  sequence $1 >>= \incl -> 
  let (inclFilenames,inclStrs) = unzip incl in
  let inclPrims = runP (concat inclStrs) parseLPrimDecl in
	  return $ Prog inclFilenames inclPrims (reverse $2) }

-------LInclude------------------
LInclude::{[IO (String,String)]}
LInclude: {- empty -} {[]}
  | LInclude1 {$1}

LInclude1: Include {[$1]}
  | LInclude1 Include {$2:$1}

Include: '#' 'include' stringLit {
--  putStrLn ("Include..."++$3) >> 
  readFile $3 >>= \contents ->  
  return ($3,(' ':contents))
}

-------LPrimDecl-----------------
LPrimDecl: {- empty -}      { [] }
  | LPrimDecl1    {$1}
  
LPrimDecl1: PrimDecl    {[$1]}
  | LPrimDecl1 PrimDecl {$2:$1}
  
PrimDecl: 
-- PrimDecl with StrongPost and others
    AnnTy lit '(' LTypedParam ')'
    where FormulaWithBkt ',' '{' LLabelledFormula '}' ',' '{' LLabelledExp '}'
  { PrimDecl {primParams=((PassByVal,$1,$2):(reverse $4)),
              primPost=$7,
              primPres=(reverse $10),
              primTests=(reverse $14)}
  }

-------LMethDecl-----------------
LMethDecl: {- empty -}      { [] }
  | LMethDecl1            { $1 }

LMethDecl1:  MethDecl		{ [$1] }
	| LMethDecl1 MethDecl	{ $2:$1 }

MethDecl: 
-- MethDecl with 1 postcondition (StrongPost) and other formulae (input file for checking)
    Ref AnnTy lit '(' LTypedParam ')'
    where FormulaWithBkt ',' '{' LLabelledFormula '}' ',' '{' LQLabel '}' ',' FormulaWithBkt ',' EB
  {MethDecl {methParams=(($1,$2,$3):(reverse $5)),
             methExternal=False,
             methPost=[$8,fTrue,fTrue],
             methPres=(reverse $11),
             methUpsis=(reverse $15),
             methInv=$18,
             methOK=FormulaBogus,methERRs=[],
             methNEVER=FormulaBogus,methMUSTs=[],methMAY=([],FormulaBogus),
             methBody=$20}
  }
-- MethDecl with 3 postconditions (StrongPost,WeakPost,CondPost) and other formulae (input file for checking)
  | Ref AnnTy lit '(' LTypedParam ')'
    where FormulaWithBkt ',' FormulaWithBkt ',' FormulaWithBkt ',' '{' LLabelledFormula '}' ','
     '{' LQLabel '}' ',' FormulaWithBkt ',' EB
  { MethDecl {methParams=(($1,$2,$3):(reverse $5)),
              methExternal=False,
              methPost=[$8,$10,$12],
              methPres=(reverse $15),
              methUpsis=(reverse $19),
              methInv=$22,
              methOK=FormulaBogus,methERRs=[],
              methNEVER=FormulaBogus,methMUSTs=[],methMAY=([],FormulaBogus),
              methBody=$24}
  }
-- MethDecl with postcondition + precondition and no other formulae (input file for checking, easier to read)
  | Ref AnnTy lit '(' LTypedParam ')'
    where FormulaWithBkt ',' '{' LLabelledFormula '}' EB
  { MethDecl {methParams=(($1,$2,$3):(reverse $5)),
              methExternal=False,
              methPost=[$8,fTrue,fTrue],
              methPres=(reverse $11),
              methUpsis=[],
              methInv=FormulaBogus,
              methOK=FormulaBogus,methERRs=[],
              methNEVER=FormulaBogus,methMUSTs=[],methMAY=([],FormulaBogus),
              methBody=$13}
  }
-- MethDecl without postcondition and other formulae (input file for inference)
  | Ref AnnTy lit '(' LTypedParam ')'EB 
  { MethDecl {methParams=(($1,$2,$3):(reverse $5)),
              methExternal=False,
              methPost=(triple FormulaBogus),
              methPres=[],
              methUpsis=[],
              methInv=FormulaBogus,
              methOK=FormulaBogus,methERRs=[],
              methNEVER=FormulaBogus,methMUSTs=[],methMAY=([],FormulaBogus),
              methBody=$7}
  }

Ref: {- empty -} { PassByVal }
  | ref { PassByRef }

LQLabel: {- empty -} {[]}
  | LQLabel1      {$1}

LQLabel1: QLabel  {[$1]}
  | LQLabel1 ',' QLabel   {$3:$1}

QLabel: Label  {[$1]}
  | QLabel '.' Label  {$3:$1}

Label: lit {$1}

EB: '{' LVarDecl Exp '}'
	{ ExpBlock (reverse $2) $3 }

-------LTypedParam-----------------
LTypedParam:	{- empty -}		{ [] }
	| LTypedParam1				{ $1 }
LTypedParam1: TypedParam				{ [$1] }
	| LTypedParam1 ',' TypedParam		{$3:$1}

TypedParam: Ref AnnTy lit	{ ($1,$2,$3) }
-------LLabelledFormula---------------
LLabelledFormula: {- empty -}      {[]}
  | LLabelledFormula1    {$1}
  
LLabelledFormula1: LabelledFormula    {[$1]}
  | LLabelledFormula1 ',' LabelledFormula  {$3:$1} 
  
LabelledFormula: QLabel ':' FormulaWithBkt
  {((reverse $1),$3)}
-------LLabelledExp-------------
LLabelledExp: {- empty -}      {[]}
  | LLabelledExp1    {$1}
  
LLabelledExp1: LabelledExp    {[$1]}
  | LLabelledExp1 ',' LabelledExp  {$3:$1} 
  
LabelledExp: Label ':' Exp
  {($1,$3)}
-------LVarDecl-----------------
LVarDecl: {- empty -} 		{ [] }
	| LVarDecl VarDecl ';'	{$2:$1}

VarDecl: AnnPrimTy lit	OptInit 	{VarDecl $1 $2 $3}
  | AnnArrTy lit OptInit
      {VarDecl $1 $2 $3}
  | Label ':' AnnArrTy '[' LVarorIntNum ']' lit OptInit
      {LblArrVarDecl (Just $1) $3 (reverse $5) $7 $8}
  | AnnArrTy '[' LVarorIntNum ']' lit OptInit
      {LblArrVarDecl Nothing $1 (reverse $3) $5 $6}
-------LVarorIntNum-------------
LVarorIntNum: LVar1     { map (\v -> ExpVar v) $1 }
  | LIntNum   { $1 }
LVar1 : Var				{ [$1] }
	| LVar1 ',' Var		{$3:$1}
LIntNum : intNum		{[KIntNum $1]}
	| LIntNum ',' intNum	{KIntNum $3:$1}

OptInit: {- empty -}    {ExpBogus}
    | ':=' Exp %prec ASSIGN          {$2}

AnnTy : AnnPrimTy		{$1}
	| AnnArrTy   {$1}

AnnPrimTy : void  {PrimVoid{anno=Nothing}}
	| float		{PrimFloat{anno=Nothing}}
	| int	OptAnn	{PrimInt{anno=$2}}
	| bool OptAnn {PrimBool{anno=$2}}

OptAnn: {- empty -} {Nothing}
  | '<' lit '>'  {(Just $2)}
  
AnnArrTy: AnnPrimTy '[' LAnnPrimTy ']' 
  {ArrayType $1 (reverse $3)}

LAnnPrimTy: AnnPrimTy         {[$1]}
  | LAnnPrimTy ',' AnnPrimTy  {$3:$1}

-------LExp---------------------
LExp :	{- empty -}		{ [] }
	| LExp1				{ $1 }
LExp1 : Exp				{ [$1] }
	| LExp1 ',' SimpleExp		{$3:$1}

Exp: SimpleExp		{$1}
	| Exp ';' Exp	 {Seq $1 $3}
	| if SimpleExp then EB else EB  %prec IF {If False $2 $4 $6}
	| ifnd SimpleExp then EB else EB  %prec IF {If True $2 $4 $6}
	| while SimpleExp do  EB %prec IF { While $2 $4 }
	| for '(' SimpleExp ';' SimpleExp ';' SimpleExp ')' EB %prec IF { For $3 $5 $7 $9}
	| EB {$1}

SimpleExp: Const {$1}
	| '(' SimpleExp ')'	{$2}
	| Var ':=' SimpleExp	%prec ASSIGN {AssignVar $1 $3}
	| Var			{ExpVar $1}
	| Label ':' Var '(' LExp ')' {LblMethCall (Just $1) $3 (reverse $5)}
	| Var '(' LExp ')' {LblMethCall Nothing $1 (reverse $3)}
	| '-' SimpleExp %prec NEG  
	  {LblMethCall (Nothing) "minus" ((KIntNum 0):[$2])}
	| 'error'   {ExpError}
-- sugar
  | SimpleExp '+' SimpleExp
    { LblMethCall (Nothing) "plus" ($1:[$3])}
    --	{Plus $1 $3}
  | SimpleExp '-' SimpleExp
    { LblMethCall (Nothing) "minus" ($1:[$3])}
  | SimpleExp '*' SimpleExp
    { LblMethCall (Nothing) "mul" ($1:[$3])}
  | SimpleExp '/' SimpleExp
    { LblMethCall (Nothing) "divide" ($1:[$3])}
  | SimpleExp '=' SimpleExp
    { LblMethCall (Nothing) "eq" ($1:[$3])}
  | SimpleExp '<' SimpleExp
    { LblMethCall (Nothing) "lt" ($1:[$3])}
  | SimpleExp '>' SimpleExp
    { LblMethCall (Nothing) "gt" ($1:[$3])}
  | SimpleExp '>=' SimpleExp
    { LblMethCall (Nothing) "gte" ($1:[$3])}
  | SimpleExp '<=' SimpleExp
    { LblMethCall (Nothing) "lte" ($1:[$3])}
	| Label ':' Var '[' LExp ']' ':=' SimpleExp %prec ASSIGN 
	  { LblMethCall (Just $1) "assign" ((ExpVar $3):(reverse ($8:$5))) }
	| Var '[' LExp ']' ':=' SimpleExp %prec ASSIGN 
	  { LblMethCall Nothing "assign" ((ExpVar $1):(reverse ($6:$3))) }
	| Label ':' Var '[' LExp ']'    
	  { LblMethCall (Just $1) "sub" ((ExpVar $3):(reverse $5)) }
	| Var '[' LExp ']'    
	  { LblMethCall Nothing "sub" ((ExpVar $1):(reverse $3)) }

Var : lit {$1}

Const : true {KTrue}
	| false {KFalse}
	| Num {$1}
	| void {KVoid}

Num : intNum {KIntNum $1}
	| floatNum {KFloatNum $1}

FormulaWithBkt: '(' Formula ')' {$2}

Formula: QFormula  {$1}
  | '(' Formula ')' {$2}
  | Formula '&&' Formula 
    { And [$1,$3] }
  | Formula '||' Formula 
    { Or [$1,$3] }
  | true { fTrue }
  | false { fFalse }
  | bogus { FormulaBogus }


QFormula:: {Formula}
QFormula: LBExpr { let (f,rest)=$1 in f}
  | exists '(' LPorUSizeVar ':' Formula ')' 
    { fExists (reverse $3) $5 }
  | forall '(' LPorUSizeVar ':' Formula ')' 
    { fForall (reverse $3) $5 }
  
-- from the final result of qs, only Formula is useful
-- [[Update]] is needed only in the intermediate productions
LBExpr:: {(Formula,[[Update]])}
LBExpr:
  BExpr {$1}
  | LBExpr RelOp LAExpr
  { let (f,rest) = $1 in
    let third = reverse $3 in
    let combi = [(e1,e2) | e1 <- rest, e2 <- third] in
      case $2 of
        TkEq  -> 
          let newfs = map (\(e1,e2) -> EqK (e1 ++ (minus_update e2))) combi in 
            (And (f:newfs),third)
        TkGTE -> 
          let newfs = map (\(e1,e2) -> GEq (e1 ++ (minus_update e2))) combi in 
            (And (f:newfs),third)
        TkGT  ->
          let newfs = map (\(e1,e2) -> GEq ((Const (-1)):(e1 ++ minus_update e2))) combi in
            (And (f:newfs),third)
        TkLTE ->
          let newfs = map (\(e1,e2) -> GEq (e2 ++ (minus_update e1))) combi in
            (And (f:newfs),third)
        TkLT  ->
          let newfs = map (\(e1,e2) -> GEq ((Const (-1)):(e2 ++ minus_update e1))) combi in
            (And (f:newfs),third)
  }


BExpr:: { (Formula,[[Update]]) }
BExpr: 
  LAExpr RelOp LAExpr
  { let (first,third) = (reverse $1,reverse $3) in
    let combi = [(e1,e2) | e1 <- first, e2 <- third] in
    case $2 of
      TkEq -> 
        let newfs = map (\(e1,e2) -> (EqK (e1 ++ (minus_update e2)))) combi in
          if singleton newfs then (head newfs,third) else (And newfs,third)
      TkGTE -> 
        let newfs = map (\(e1,e2) -> (GEq (e1 ++ (minus_update e2)))) combi in 
          if singleton newfs then (head newfs,third) else (And newfs,third)
      TkGT  -> 
        let newfs = map (\(e1,e2) -> (GEq ((Const (- 1)):(e1 ++ (minus_update e2))) )) combi in
          if singleton newfs then (head newfs,third) else (And newfs,third)
      TkLTE -> 
        let newfs = map (\(e1,e2) -> (GEq (e2 ++ (minus_update e1)))) combi in 
          if singleton newfs then (head newfs,third) else (And newfs,third)
      TkLT  -> 
        let newfs = map (\(e1,e2) -> (GEq ((Const (- 1)):(e2 ++ (minus_update e1))) )) combi in
          if singleton newfs then (head newfs,third) else (And newfs,third)
  }
    
RelOp:: {Tk}
RelOp: '='  {$1}
  | '>='  {$1}
  | '>'   {$1}
  | '<='  {$1}
  | '<'   {$1}

-- list of AExpr -- AExpr,AExpr,..
LAExpr:: { [[Update]]}
LAExpr: LAExpr ',' AExpr   { $3:$1 }
  | AExpr  { [$1] }

      
-- AExpr == [Update]
AExpr:: { [Update] }
AExpr : AExpr '+' AExpr { $1 ++ $3 }
     | AExpr '-' AExpr { $1 ++ (minus_update $3) }
     | '(' AExpr ')'  { $2 }
     | intNum PorUSizeVar  { [ Coef $2 $1 ] }
     | '-' intNum PorUSizeVar  { [ Coef $3 (-$2) ] }
     | intNum           { [ Const $1 ] }
     | '-' intNum %prec NEG { [ Const (- $2)] }
     | PorUSizeVar      { [ Coef $1 1 ]} 
     | '-' PorUSizeVar      { [ Coef $2 (-1) ]} 

LPorUSizeVar: LPorUSizeVar ',' PorUSizeVar {$3:$1}
  | PorUSizeVar     {[$1]}

PorUSizeVar:: {QSizeVar}
PorUSizeVar: 
  lit {(stringToQsv $1)}
  | lit prime {(SizeVar $1,Primed)}
  | lit rec {(SizeVar $1,Recursive)}
  | lit '.' lit { 
      if ($3=="min") 
        then ((ArrSizeVar $1 Min),Unprimed)
        else 
          if ($3=="max") 
            then ((ArrSizeVar $1 Max),Unprimed) 
            else error $ "neither min or max after QSizeVar"
      }
  | lit '.' lit prime { 
      if ($3=="min") 
        then ((ArrSizeVar $1 Min),Primed)
        else 
          if ($3=="max") 
            then ((ArrSizeVar $1 Max),Primed) 
            else error $ "neither min or max after QSizeVar"
      }
  | lit '.' lit rec { 
      if ($3=="min") 
        then ((ArrSizeVar $1 Min),Recursive)
        else 
          if ($3=="max") 
            then ((ArrSizeVar $1 Max),Recursive) 
            else error $ "neither min or max after QSizeVar"
      }


{
happyError :: P a
happyError = do l <- getLineNum
		s <- getInput
		error $ "Parse error on line " ++ (show l) ++ " rest of line: " ++ (takeWhile (/= '\n') s)

minus_update :: [Update] -> [Update]
minus_update [] = []
minus_update ((Const i):us) = (Const (- i)):(minus_update us)
minus_update ((Coef v i):us) = (Coef v (- i)):(minus_update us) 

--returning type varies depending on the grammar's start symbol
parse :: String -> IO Prog
parse s = runP s parseProg 
--parseFormula s = runP s parseFormula1
}
