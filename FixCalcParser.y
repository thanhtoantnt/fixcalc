{
module FixCalcParser where
import ImpAST
import ImpConfig(defaultFlags,Flags(..),Heur(..))
import ImpFixpoint2k(bottomUp2k,bottomUp2k_gen,bottomUp_mr,topDown2k,gfp2k,subrec_z)
import ImpFixpoint2k(subrec_z_mut,subrec_gen,combSelHull,getDisjuncts,widen)
import ImpHullWiden(narrow)
import ImpFixpoint2k(fixTestBU,fixTestTD,getOneStep,getEq,pickEqFromEq)
import ImpFixpoint2k(pickGEQfromEQ,fixTestBU_Lgen,satEQfromEQ,satGEQfromEQ)
import ImpFormula(simplify,subset,difference,complement,pairwiseCheck,hull,apply,debugApply)
import Fresh
import FixCalcLexer(runP,P(..),Tk(..),lexer,getLineNum,getInput)
import MyPrelude
------------------------------------------
import Data.List(nub,elemIndex,transpose)
import Data.Maybe(fromJust)
import Control.Monad(foldM)
}

%monad {P}
%lexer {lexer} {TkEOF}
%tokentype {Tk}
%token
  lit                     {TkAlphaNum $$}
  intNum                  {TkIntNum $$}
  true                    {TkTrue}
  false                   {TkFalse}
  '+'                     {TkPlus}
  '-'                     {TkMinus}
  '('                     {TkLBr}
  ')'                     {TkRBr}
  ';'                     {TkSemiColon}
  ':='                    {TkAssign}
  '['                     {TkLSqBr}
  ']'                     {TkRSqBr}
  '{'                     {TkLAcc}
  '}'                     {TkRAcc}
  ','                     {TkComma}
  '='                     {TkEq}
  '<'                     {TkLT}
  '>'                     {TkGT}
  '>='                    {TkGTE}
  '<='                    {TkLTE}
  '&&'                    {TkAnd}
  '||'                    {TkOr}
  ':'                     {TkColon}
  '.'                     {TkDot}
  '!'                     {TkNot}
  exists                  {TkExists}
  forall                  {TkForall}
  prime                   {TkPrime}
  rec                     {TkRec}
  apply                   {TkKwApply}
  widen                   {TkKwWiden}
  narrow                  {TkKwNarrow}
  subset                  {TkKwSubset}
  complement              {TkKwComplement}
  bottomup                {TkKwBottomup}
  bottomup_mr             {TkKwBottomup_mr}
  bottomup_gen            {TkKwBottomup_gen}
  topdown                 {TkKwTopdown}
  gfp                     {TkKwGFP}
  selhull                 {TkKwSelhull}
  manualhull              {TkKwManualhull}
  intersection            {TkKwIntersection}
  pairwisecheck           {TkKwPairwisecheck}
  hull                    {TkKwHull}
  fixtestpost             {TkKwFixtestpost}
  fixtestinv              {TkKwFixtestinv}
  pickEqFromEq            {TkKwPickEqFromEq}
  pickGEqFromEq           {TkKwPickGEqFromEq}
  SatEQfromEQ             {TkKwSatEQfromEQ}
  SatGEQfromEQ            {TkKwSatGEQfromEQ}
%left '||'
%left '&&'
%nonassoc '>' '<' '>=' '<=' '='
%left ','         -- , must be higher than < but lower than + because 1+x<y,z+2
%left '+' '-'
%left NEG

%name parseCalc LCommand
%%


LCommand:: {[RelEnv -> FS RelEnv]}
LCommand: 
  Command LCommand       {$1:$2}
  | {- empty -}              {[]}

Command::{RelEnv -> FS RelEnv}
Command:
    lit ':=' ParseFormula ';'
    {\env -> putStrNoLnFSOpt ("# " ++ $1 ++ ":=") >>
             $3 env >>= \rhs ->
             putStrFS_debug ("# " ++ $1 ++ ":=" )>>
             case rhs of {
               R (RecPost _ f triple) -> 
                 return (R (RecPost $1 f triple)); 
               F f -> 
                 simplify f >>= \sf -> 
                 return (F sf);
               QF (qv,f) ->
                 simplify f >>= \sf ->
                 return (QF (qv,sf))
             } >>= \renamedRHS ->
                 putStrFS_debug ("#bottomup " ++ $1 ++ ":=") >>
                 return (extendRelEnv env ($1,renamedRHS))}
  | ParseFormula2 ';'
    {\env -> $1 env >>= \res ->
               return res
    }
  |  '[' Llit2 ']' ':=' ParseFormula1 ';'
    {\env -> 
        $5 env >>= \fl ->
        if (length fl /= length $2)
        then 
            error "Mismatch in number of LHS and RHS"
        else 
           let new_fl = zip $2 fl in
             mapM (\(id,rhs) ->
               case rhs of {
                 R (RecPost _ f triple) ->
                   --putStrFS_debug("bach_f_rec="++ show f) >>  
                   return (R (RecPost id f triple)); 
                 (F f) -> 
                   --putStrFS_debug("bach_f="++ show f) >>  
                   simplify f >>= \fsimpl -> 
                   --putStrFS(show fsimpl) >>  
                   return (F fsimpl);
                 (QF (qv,f)) -> 
                   --putStrFS_debug("bach_f="++ show f) >>  
                   simplify f >>= \fsimpl -> 
                   --putStrFS(show fsimpl) >>  
                   return (QF (qv,fsimpl))}
             ) new_fl >>= \rhs1 -> 
           let rhs_new = zip $2 rhs1 in
           foldM (\env1 -> \(id,rhs2) ->
              case rhs2 of
                 F f -> 
                  --putStrFS_debug ("#bach_gen " ++ id ++ ":="++(show f)++"\n") >>
                  putStrNoLnFSOpt ("# " ++ id ++ ":="++(show f)++"\n") >>
                  return (extendRelEnv env1 (id,rhs2))
                 _ -> error "impossible : should be a formula"
                ) env rhs_new
    }
  | ParseFormula1 ';'
    {\env -> $1 env >>= \fl -> 
         mapM (\rhs ->
             case rhs of
               (F f) -> 
                 simplify f >>= \fsimpl -> 
                 putStrFS(show fsimpl) >> 
                 return (F fsimpl)
               (QF (qv,f)) -> 
                 simplify f >>= \fsimpl -> 
                 putStrFS(show (qv,fsimpl)) >> 
                 return (QF (qv,fsimpl))
               (R recpost) -> 
                 putStrFS(show recpost) >> 
                 return rhs
             ) fl >>= \rhs1 -> 
         foldM (\env1 -> \rhs2 -> 
       return (extendRelEnv env1 (" ",rhs2))) env rhs1
     }
  | ParseFormula ';'
    {\env -> $1 env >>= \rhs -> 
             case rhs of
               (F f) -> 
                  simplify f >>= \fsimpl ->
                  putStrFSOpt("\n" ++ showSet fsimpl ++ "\n") >> 
                  return env
               (QF (qv,f)) -> 
                  simplify f >>= \fsimpl ->
                  putStrFSOpt("\n{[" ++ show qv ++ "] : " ++  showSet fsimpl ++ "}\n") >> 
                  return env
               (R recpost) -> 
                  putStrFS ("\n" ++ show recpost ++ "\n") >> 
                  return env
    }
  {-| fixtestpost '(' lit ',' lit ')' ';' --fixtestpost '(' '['Llit ']'',' '['Llit ']'')' ';'
    {\env -> putStrFS("# fixtestPost("++ $3 ++ "," ++ $5 ++ ");") >> 
             case (lookupVar $3 env,lookupVar $5 env) of
               (Just (R recpost),Just (F f)) ->
                  fixTestBU recpost f >>= \fixok -> 
                  putStrFSOpt("\n# " ++ show fixok ++ "\n") >> 
                  return env
               (_,_) -> error ("Arguments of fixtest are incorrect")}
  -}
  | fixtestpost '(' '['Llit2 ']'',' '['Llit2 ']'')' ';'
    {\env -> --putStrFS("# fixtestPost("++ (show $4) ++ "," ++ (show $8) ++ ");") >> 
        if(length $4 ==length $8) 
        then
          let (qvr,rcp) = unzip (map (\x -> case lookupVar x env of
                            Just (R recpost@(RecPost _ _ (sv1,sv2,_)))-> ((sv1++sv2),recpost)
                            _ ->  error ("Arguments of fixtest are incorrect")) $4)
          in
          let mf = map (\(x,qv1) -> case lookupVar x env of
                    Just (F f) -> f;
                    Just (QF (qv2,f)) ->
                      let subs = zip qv2 qv1 in
                      apply subs f
                    _->  error ("Arguments of fixtest are incorrect")) $ (zip $8 qvr) 
          in
          fixTestBU_Lgen rcp mf >>= \fixok ->
          putStrFSOpt("\n# " ++ show fixok ++ "\n") >>
          return env
        else 
          error ("Mismatch numbers of [] and [] in RHS!")
    }
  | fixtestinv '(' lit ',' lit ')' ';'
    {\env -> putStrFS("# fixtestInv("++ $3 ++ "," ++ $5 ++ ");") >> 
             case (lookupVar $3 env,lookupVar $5 env) of
               (Just (R recpost),Just (F f)) ->
                  getOneStep recpost fTrue >>= \oneStep ->
                  fixTestTD oneStep f >>= \fixok -> 
                  putStrFSOpt("\n# " ++ show fixok ++ "\n") >> 
                  return env
               (Just (R recpost@(RecPost _ _ (sv1,sv2,_))),Just (QF (qv2,f))) ->
                  getOneStep recpost fTrue >>= \oneStep ->
                  let qv1 = sv1++sv2 in
                  let subs = zip qv2 qv1 in
                  let sf = apply subs f in
                  fixTestTD oneStep sf >>= \fixok -> 
                  putStrFSOpt("\n# " ++ show fixok ++ "\n") >> 
                  return env
               (_,_) -> error ("Arguments of fixtestInv are incorrect")}
  | lit subset lit ';' 
    {\env -> putStrFSOpt("# "++ $1 ++ " subset " ++ $3 ++ ";") >>
             case (lookupVar $1 env,lookupVar $3 env) of
               (Just (F f1),Just (F f2)) ->
                 subset f1 f2 >>= \subok -> 
                 putStrFSOpt("\n# " ++ show subok ++ "\n") >> 
                 return env
               (Just (QF (qv1,f1)),Just (QF (qv2,f2))) ->
                 if (length qv1) == (length qv2) then
                   let subs = zip qv2 qv1 in
                   let sf2 = apply subs f2 in
                   subset f1 sf2 >>= \subok -> 
                   putStrFSOpt("\n# " ++ show subok ++ "\n") >> 
                   return env
                 else error ("Arguments of subset are not valid QFormulas\n")
               (_,_) -> error ("Arguments of subset are not valid\n")
     }
 
  | lit ';'
    {\env -> putStrFSOpt("\n# "++ $1 ++ ";") >>
             case lookupVar $1 env of 
               Just (R recpost) -> putStrFS("\n" ++ show recpost ++ "\n") >> 
                 return env
               Just (F f) -> putStrFS("\n" ++ show f ++ "\n") >> 
                 return env
               Just (QF qf) -> putStrFS("\n" ++ show qf ++ "\n") >> 
                 return env
               Nothing -> error ("# Variable not declared - "++$1++"\n")
    }


ParseFormula1::{RelEnv -> FS [Value]}
ParseFormula1:
  bottomup_gen '(' '[' Llit ']' ',' '[' LInt ']' ',' lit ')' 
    {\env -> 
      let heur = case $11 of {"SimHeur" -> SimilarityHeur; 
                             "DiffHeur" -> DifferenceHeur; 
                             "HausHeur" -> HausdorffHeur; 
                             "InterHeur" -> SimInteractiveHeur; 
                             lit -> error ("Heuristic not implemented parser.y - "++lit)} 
      in
      bottomUp2k_gen ($4 env) (map (\x -> (x,heur)) ($8)) (map (\x -> fFalse) ($4 env)) 
      >>= \resl -> return (map (\x -> F x) (fst (unzip resl)))}

  | apply '(' '[' Llit2 ']' ',' '[' Llit2 ']' ')'
    {\env ->
        putStrFSOpt ("apply(" ++ show $4 ++ "," ++ show $8 ++ ");") >>
        if(length $4 ==length $8) then
            let qv_rp = map (\x -> case lookupVar x env of {
                Just (R recpost@(RecPost _ _ (sv1,sv2,_)))-> ((sv1++sv2),recpost);
                _ ->  error ("apply: mismatched argume")}) $4 in
            let (mqv,mrp) = unzip qv_rp in
            let mf = map (\(x,qv1) -> case lookupVar x env of {
                Just (F f) -> f;
                Just (QF (qv2,f)) ->
                    let subs = zip qv2 qv1 in
                    apply subs f;
                 _->  error ("apply: mismatched arguments of relation")
                }) $ (zip $8 mqv) in
            subrec_gen mrp mf  >>= \fn ->
            mapM (\x -> simplify x >>= \f -> return (F f)) fn >>= \fs ->
            return fs
        else error ("apply: mismatched arguments of relations!")
    }
  | selhull '(' '[' Llit2 ']' ',' '[' LInt ']' ',' '[' Llit2 ']' ')'
    {\env ->
        putStrFSOpt ("selhull(" ++ show $4 ++ "," ++ show $8 ++ "," ++ show $12 ++ ");") >>
        if ((length $4) == (length $8)) && ((length $4) == (length $8)) then
            let params = zip3 $4 $8 $12 in
            mapM (\ (id,val,hr) -> case lookupVar id env of {
                Just (R recpost) -> error ("Argument of selhull is not a formula\n");
                Nothing -> error ("Variable not declared - "++ show id ++"\n");
                Just (QF qf) -> error ("Argument of selhull is not a formula\n");
                Just (F f) ->
                    let heur = case hr of {
                        "SimHeur" -> SimilarityHeur;
                        "DiffHeur" -> DifferenceHeur;
                        "HausHeur" -> HausdorffHeur;
                        lit -> error ("Heuristic not implemented parser.y4 - "++lit)
                    } in
                    combSelHull (val,heur) (getDisjuncts f) [] >>= \disj -> return (F (Or disj))
                }) params
        else error ("selhull: invalid arguments")
    }
  | widen '(' '[' Llit2 ']' ',' '[' Llit2 ']' ',' '[' Llit2 ']' ')'
    {\env ->
        putStrFSOpt ("widen(" ++ show $4 ++ "," ++ show $8 ++ "," ++ show $12 ++ ");") >>
        if ((length $4) == (length $8)) && ((length $4) == (length $8)) then
            let params = zip3 $4 $8 $12 in
            mapM (\ (val1,val2,hr) ->
                case (lookupVar val1 env, lookupVar val2 env) of {
                    (Just (F f1), Just (F f2)) ->
                        let heur = case hr of {
                            "SimHeur" -> SimilarityHeur;
                            "DiffHeur" -> DifferenceHeur;
                            "HausHeur" -> HausdorffHeur;
                            lit -> error ("Heuristic not implemented parser.y4 - "++lit)
                        } in
                        widen heur [] (getDisjuncts f1,getDisjuncts f2) >>= \disj ->
                       return (F (Or disj));
                    (_,_) -> error "widen: invalid arguments"
               }) params
        else error ("widen: invalid arguments")
    }

ParseFormula2::{RelEnv -> FS RelEnv}
ParseFormula2:
  lit ':=' pickEqFromEq '(' lit ')'
  {\env ->
      case (lookupVar $5 env) of 
        Just (F f) -> 
          simplify f >>= \f1 ->
          return f1
        _ -> error ("PickEqFromEq sorely supports Formula")
      >>= \fl -> 
      putStrFS_debug("#After parse Formula: "++show (fl)) >>
      let rs1=getEq fl in
      putStrFS_debug("#getEq: "++show (rs1)) >>
      let eq_udt_list =pickEqFromEq rs1 in
      putStrFS_debug("#list eq after pick="++show (eq_udt_list)) >>
      let rhs=concat (map (\x -> return (EqK x)) eq_udt_list) in
      putStrFS_debug("#concat="++show (rhs)) >>
      --foldM (\env1 -> \rhs1 -> return (extendRelEnv env1 ($1,(F rhs1)))) env rhs  --formula in which are disj or conj => needs to be modified here?     
      return (extendRelEnv env ($1,(F (And rhs))))
      --return env
   }
 |  pickEqFromEq '(' lit ')'
  {\env ->
      case (lookupVar $3 env) of 
        Just (F f) -> 
          simplify f >>= \f1 ->
          return f1
        _ -> error ("PickEqFromEq solely supports Formula")
      >>= \fl -> 
      putStrFS_debug("#After parse Formula: "++show (fl)) >>
      let rs1=getEq fl in
      putStrFS_debug("#getEq: "++show (rs1)) >>
      let eq_udt_list =pickEqFromEq rs1 in
      putStrFS_debug("#list eq after pick="++show (eq_udt_list)) >>
      let rhs=concat (map (\x -> return (EqK x)) eq_udt_list) in
      putStrFS_debug("#concat="++show (rhs)) >>   
      putStrFS_DD 0 ("# pickEqFromEq("++$3++")") >>
      putStrFS(show (And rhs)) >>
      return (extendRelEnv env (" ",(F (And rhs))))
      --return env
   }  
 | lit ':=' pickGEqFromEq '(' lit ')'
  {\env ->
      case (lookupVar $5 env) of 
        Just (F f) -> 
          simplify f >>= \f1 ->
          return f1
        _ -> error ("PickGEqFromEq sorely supports Formula")
      >>= \fl -> 
      putStrFS_debug("#After parse Formula GEq: "++show (fl)) >>
      pickGEQfromEQ fl >>= \gEq ->
      --mapM (\g1 ->putStrFS_debug("#list GEq after pick="++show (g1))) gEq >>
      let rhs=concat (mapM (\x -> return x) gEq) in
      putStrFS_debug("#concat="++show (rhs)) >>    
      return (extendRelEnv env ($1,(F (And rhs))))
      --return env
   }
 | pickGEqFromEq '(' lit ')'
  {\env ->
      case (lookupVar $3 env) of 
        Just (F f) -> 
          simplify f >>= \f1 ->
          return f1
        _ -> error ("PickGEqFromEq sorely supports Formula")
      >>= \fl -> 
      putStrFS_debug("#After parse Formula GEq: "++show (fl)) >>
      pickGEQfromEQ fl >>= \gEq ->
      --mapM (\g1 ->putStrFS_debug("#list GEq after pick="++show (g1))) gEq >>
      let rhs=concat (mapM (\x -> return x) gEq) in
      putStrFS_debug("#concat="++show (rhs)) >>
      putStrFS("#pickGEqFromEq of "++$3++" : "++show (And rhs)) >>     
      return (extendRelEnv env (" ",(F (And rhs))))
      --return env
   }
 | lit ':=' SatEQfromEQ '(' lit ')'
  {\env -> 
       case (lookupVar $5 env) of 
        Just (F f) -> 
          simplify f >>= \f1 ->
          return f1
        _ -> error ("satEQfromEQ sorely supports Formula")
      >>= \fl -> 
      --putStrFS_debug("#satEQEQ After parse Formula: "++show (fl)) >>
      satEQfromEQ fl >>= \fsatEQ -> return (extendRelEnv env ($1,(F (And fsatEQ))))
      --return env
   }
 | SatEQfromEQ '(' lit ')'
  {\env -> 
       case (lookupVar $3 env) of 
        Just (F f) -> 
          simplify f >>= \f1 ->
          return f1
        _ -> error ("satEQfromEQ sorely supports Formula")
      >>= \fl -> 
      --putStrFS_debug("#satEQEQ After parse Formula: "++show (fl)) >>
      satEQfromEQ fl >>= \fsatEQ -> return (extendRelEnv env (" ",(F (And fsatEQ))))
      --return env
   }  
 | lit ':=' SatGEQfromEQ '(' lit ')'
  {\env ->
      case (lookupVar $5 env) of 
        Just (F f) -> 
          putStrFS("EQEQ org:"++ show f)>>
          simplify f >>= \f1 ->
          putStrFS("EQEQ sim:"++ show f1)>>
          return f1
        _ -> error ("satGEQFromEQ sorely supports Formula")
      >>= \fl -> 
      --putStrFS_debug("#saeGEQEQ After parse Formula GEq: "++show (fl)) >>
      satGEQfromEQ fl >>= \rhs -> return (extendRelEnv env ($1,(F (And rhs))))
      --return env
   }
 | SatGEQfromEQ '(' lit ')'
  {\env ->
      case (lookupVar $3 env) of 
        Just (F f) -> 
          simplify f >>= \f1 ->
          return f1
        _ -> error ("satGEQFromEQ sorely supports Formula")
      >>= \fl -> 
      --putStrFS_debug("#saeGEQEQ After parse Formula GEq: "++show (fl)) >>
      satGEQfromEQ fl >>= \rhs -> return (extendRelEnv env (" ",(F (And rhs))))
      --return env
   }        
ParseFormula::{RelEnv -> FS Value}
ParseFormula:
    '{' Formula '}'      
                  {\env -> putStrFSOpt ("{ ... };") >>
                           if "f_" `elem` (map (\(SizeVar anno,_) -> take 2 anno) (fqsv $2)) then 
                             error ("Free variables of formula should not start with \"f_\" (\"f_\" are fresh variables)")
                           else return (F $2)} 
  | '{' '[' LPorUSizeVar ']' ':' Formula '}'      
                  {\env -> putStrFSOpt ("{ ... };") >>
                           if "f_" `elem` (map (\(SizeVar anno,_) -> take 2 anno) (fqsv $6)) then 
                             error ("Free variables of formula should not start with \"f_\" (\"f_\" are fresh variables)")
                           else if (length $3 == 0) then return (F $6)
                           else return (QF (reverse $3,$6))}
  | '{' '[' LPorUSizeVar ']' '-' '>' '[' LPorUSizeVar ']' '-' '>' '[' LPorUSizeVar ']' ':' Formula '}'      
                  {\env -> putStrFSOpt ("{ ... };") >> 
                           if "f_" `elem` (map (\(SizeVar anno,_) -> take 2 anno) (fqsv $16)) then 
                             error ("Free variables of formula should not start with \"f_\" (\"f_\" are fresh variables)")
                           else return (R (RecPost "dummy" $16 (reverse $3,reverse $8,reverse $13)))}
  | lit '(' lit ')'
        {\env -> putStrFSOpt ($1 ++ "(" ++ $3 ++ ");") >>
                 let (cabst,qv1) = case lookupVar $1 env of {
                     Just (R recpost@(RecPost _ _ (sv1,sv2,_))) -> (recpost, sv1++sv2); 
                     Just (F f) -> error ("Argument of subrec is not a constraint abstraction\n"); 
                     Just (QF qf) -> error ("Argument of subrec is not a constraint abstraction\n"); 
                     Nothing -> error ("Variable not declared - "++$1++"\n")} in
                 case lookupVar $3 env of {
                     Just (F f) -> 
                         subrec_z cabst f >>= \fn -> simplify fn >>= \fnext -> return (F fnext);
                     Just (QF (qv2,f)) ->
                         let subs = zip qv2 qv1 in
                         let sf = apply subs f in
                         {-- print_DD True 2 [("== QF qv1 = ", show qv1)] >>
                         print_DD True 2 [("== QF qv2 = ", show qv2)] >>
                         print_DD True 2 [("== QF subs = ", show subs)] >>
                         print_DD True 2 [("== QF f   = ", show f)] >>
                         print_DD True 2 [("== QF sf  = ", show sf)] >> --}
                         subrec_z cabst sf >>= \fn -> simplify fn >>= \fnext -> return (F fnext);
                     Just (R recpost) -> error ("Argument of subrec is not a formula\n"); 
                     Nothing -> error ("Variable not declared - "++$3++"\n")
                 }}
  | lit '(' '[' Llit2 ']' ',' '[' Llit2 ']' ')'
        {\env -> putStrFSOpt ($1 ++ "([" ++ show $4 ++ "],[" ++ show $8 ++ "]);") >>
                 let (cabst,qv1) = case lookupVar $1 env of {
                     Just (R recpost@(RecPost _ _ (sv1,sv2,_))) -> (recpost, sv1++sv2); 
                     Just (F f) -> error ("Argument of subrec is not a constraint abstraction\n"); 
                     Just (QF qf) -> error ("Argument of subrec is not a constraint abstraction\n"); 
                     Nothing -> error ("Variable not declared - "++$1++"\n")} in
                 if(length $4 ==length $8) then
                     let (mqv,mrp) = unzip (map (\x -> case lookupVar x env of
                                       Just (R recpost@(RecPost _ _ (sv1,sv2,_)))-> ((sv1++sv2),recpost)
                                       _ ->  error ("Relation arguments of subrec are incorrect")) $4 )
                     in
                     let mf = map (\(x,qv1) -> case lookupVar x env of
                                Just (F f) -> f;
                                Just (QF (qv2,f)) ->
                                    let subs = zip qv2 qv1 in
                                    apply subs f
                                _->  error ("Formula arguments of subrec are incorrect")) $ (zip $8 mqv) 
                     in
                     let r_input = zip mrp mf in
                     subrec_z_mut cabst r_input >>= \fn -> simplify fn >>= \fnext -> return (F fnext);
                 else 
                     error ("Mismatch numbers of [] and [] in RHS!")
                 }
  | bottomup '(' lit ',' intNum ',' lit ')'
        {\env -> putStrFSOpt ("bottomup(" ++ $3 ++ "," ++ show $5 ++ "," ++ $7 ++ ");") >>
                 case lookupVar $3 env of
                   Just (F f) -> error ("Argument of bottomup is not a constraint abstraction\n")
                   Just (QF qf) -> error ("Argument of bottomup is not a constraint abstraction\n")
                   Nothing -> error ("Variable not declared - "++$3++"\n")
                   Just (R recpost) -> 
                       let heur = case $7 of {"SimHeur" -> SimilarityHeur; "DiffHeur" -> DifferenceHeur; "HausHeur" -> HausdorffHeur; lit -> error ("Heuristic not implemented parser.y2 - "++lit)} in
                       bottomUp2k recpost ($5,heur) fFalse >>= \(post,cnt) -> return (F post)}
  | bottomup_mr '(' lit ',' lit ')'
        {\env -> putStrFS ("bottomup_mr(" ++ $3 ++ "," ++ $5 ++ ");") >>
                 case lookupVar $3 env of
                   Just (F f) -> error ("First argument of bottomup_mr is not a constraint abstraction\n")
                   Just (QF qf) -> error ("First argument of bottomup_mr is not a constraint abstraction\n")
                   Nothing -> error ("Variable not declared - "++$3++"\n")
                   Just (R recpost1) -> 
                       case lookupVar $5 env of 
                         Just (F f) -> error ("Second argument of bottomup_mr is not a constraint abstraction\n")
                         Just (QF qf) -> error ("Second argument of bottomup_mr is not a constraint abstraction\n")
                         Nothing -> error ("Variable not declared - "++$5++"\n")
                         Just (R recpost2) -> bottomUp_mr recpost1 recpost2  >>= \(post,cnt) -> return (F post)}
  | topdown '(' lit ',' intNum ',' lit ')'
        {\env -> putStrFSOpt ("topdown(" ++ $3 ++ "," ++ show $5 ++ "," ++ $7 ++ ");") >>
                 case lookupVar $3 env of
                   Just (F f) -> error ("Argument of topdown is not a constraint abstraction\n")
                   Just (QF qf) -> error ("Argument of topdown is not a constraint abstraction\n")
                   Nothing -> error ("Variable not declared - "++$3++"\n")
                   Just (R recpost) -> 
                     let heur = case $7 of {"SimHeur" -> SimilarityHeur; "DiffHeur" -> DifferenceHeur; "HausHeur" -> HausdorffHeur; lit -> error ("Heuristic not implemented parser.y3 - "++lit)} in
                     topDown2k recpost ($5,heur) fTrue >>= \(inv,cnt) -> return (F inv)}

 | gfp '(' '[' Llit ']' ',' '[' LInt ']' ',' lit ')' 
    {\env -> 
      let heur = case $11 of {"SimHeur" -> SimilarityHeur; 
                             "DiffHeur" -> DifferenceHeur; 
                             "HausHeur" -> HausdorffHeur; 
                             "InterHeur" -> SimInteractiveHeur; 
                             lit -> error ("Heuristic not implemented parser.y - "++lit)} 
      in
      gfp2k ($4 env) (map (\x -> (x,heur)) ($8)) (map (\x -> fTrue) ($4 env)) 
        >>= \resl -> return (F (fOr (fst (unzip resl))))}

 | lit complement lit
    {\env -> putStrFSOpt("# "++ $1 ++ " complement " ++ $3 ++ ";") >>
             case (lookupVar $1 env,lookupVar $3 env) of
               (Just (F f1),Just (F f2)) ->
                 difference f1 f2 >>= \result -> 
                 return (F result)
               (_,_) -> error ("Arguments of complement are not valid\n")
     }
 | complement lit
    {\env -> putStrFSOpt("complement " ++ $2 ++ ";") >>
             case (lookupVar $2 env) of
               Just (F f1) ->
                 complement f1 >>= \result -> 
                 return (F result)
               _ -> error ("Arguments of complement are not valid\n")
     }

  | selhull '(' lit ',' intNum ',' lit ')'
        {\env -> putStrFSOpt ("selhull(" ++ $3 ++ "," ++ show $5 ++ "," ++ $7 ++ ");") >>
                 case lookupVar $3 env of
                   Just (R recpost) -> error ("Argument of selhull is not a formula\n")
                   Nothing -> error ("Variable not declared - "++$3++"\n")
                   Just (QF qf) -> error ("Argument of selhull is not a formula\n")
                   Just (F f) -> 
                     let heur = case $7 of {"SimHeur" -> SimilarityHeur; "DiffHeur" -> DifferenceHeur; "HausHeur" -> HausdorffHeur; lit -> error ("Heuristic not implemented parser.y4 - "++lit)} in
                     combSelHull ($5,heur) (getDisjuncts f) [] >>= \disj -> return (F (Or disj))}
  | manualhull '(' lit ',' '[' LInt ']' ')'
        {\env -> putStrFSOpt ("manualhull(" ++ $3 ++ "," ++ show $6 ++ ");") >>
                 case lookupVar $3 env of
                    Just (F f) -> 
                      let disj = getDisjuncts f in
                      if length disj == length $6 then
                        let grouped = groupDisjuncts (zip disj $6) (nub $6) (replicate (length (nub $6)) fFalse) in
                        mapM (\x -> hull x) grouped >>= \hulled ->
                        return (F (fOr hulled))
                      else
                        error ("Length of the list " ++ show $6 ++ " is different than the number of disjuncts in formula.")
                    _ -> error ("First argument of manualhull is not a formula.")
        }

  | narrow '(' lit ',' lit ',' lit ')' 
        {\env -> putStrFSOpt ("narrow(" ++ $3 ++ "," ++ $5 ++ "," ++ $7 ++ ");") >>
                 case (lookupVar $3 env,lookupVar $5 env) of
                   (Just (F f1),Just (F f2)) -> 
                     let heur = case $7 of {"SimHeur" -> SimilarityHeur; "DiffHeur" -> DifferenceHeur; "HausHeur" -> HausdorffHeur; lit -> error ("Heuristic not implemented parser.y5 - "++lit)} in
                     narrow heur [] (getDisjuncts f1,getDisjuncts f2) >>= \disj ->
                     return (F (Or disj))
                   (Just (R recpost),_) -> error ("Argument of narrow is not a formula\n")
                   (_,Just (R recpost)) -> error ("Argument of narrow is not a formula\n")
                   (Just (QF qf),_) -> error ("Argument of narrow is not a formula\n")
                   (_,Just (QF qf)) -> error ("Argument of narrow is not a formula\n")
                   (_,_) -> error ("Variable not declared - "++$3++"\n")
        }

  | widen '(' lit ',' lit ',' lit ')' 
        {\env -> putStrFSOpt ("widen(" ++ $3 ++ "," ++ $5 ++ "," ++ $7 ++ ");") >>
                 case (lookupVar $3 env,lookupVar $5 env) of
                   (Just (F f1),Just (F f2)) -> 
                     let heur = case $7 of {"SimHeur" -> SimilarityHeur; "DiffHeur" -> DifferenceHeur; "HausHeur" -> HausdorffHeur; lit -> error ("Heuristic not implemented parser.y5 - "++lit)} in
                     widen heur [] (getDisjuncts f1,getDisjuncts f2) >>= \disj ->
                     return (F (Or disj))
                   (Just (R recpost),_) -> error ("Argument of widen is not a formula\n")
                   (_,Just (R recpost)) -> error ("Argument of widen is not a formula\n")
                   (Just (QF qf),_) -> error ("Argument of widen is not a formula\n")
                   (_,Just (QF qf)) -> error ("Argument of widen is not a formula\n")
                   (_,_) -> error ("Variable not declared - "++$3++"\n")
        }
  | lit intersection lit
        {\env -> putStrFSOpt($1 ++ " intersection " ++ $3 ++ ";") >>
                 case (lookupVar $1 env,lookupVar $3 env) of
                   (Just (F f1),Just (F f2)) ->
                      simplify (And [f1,f2]) >>= \f3 -> 
                      return (F f3)
                   (_,_) -> error ("Argument of intersection is not a valid formula\n")
         }
  | hull lit
        {\env -> putStrFSOpt("hull " ++ $2 ++ ";") >>
                 case (lookupVar $2 env) of
                   Just (F f1) -> hull f1 >>= \f2 -> 
                      return (F f2)
                   _ -> error ("Argument of hull is not a valid formula\n")
         }
  | pairwisecheck lit
        {\env -> putStrFSOpt ("PairwiseCheck "++ $2) >>
                 case lookupVar $2 env of
                   Just (F f) -> 
                      pairwiseCheck f >>= \fsimpl ->
                      return (F fsimpl)
                   _ -> error ("Argument of pairwisecheck is not a valid formula "++$2++"\n")
        }

Llit:: {RelEnv -> [RecPost]}
Llit: lit {
     \env -> case lookupVar $1 env of
       Just (F f) -> error ("Argument of bottomup is not a constraint abstraction\n")
       Just (QF qf) -> error ("Argument of bottomup is not a constraint abstraction\n")
       Nothing -> error ("Variable not declared - "++$1++"\n")
       Just (R recpost) -> [recpost]
  }
 | lit ',' Llit {
     \env -> case lookupVar $1 env of
       Just (F f) -> error ("Argument of bottomup is not a constraint abstraction\n")
       Just (QF qf) -> error ("Argument of bottomup is not a constraint abstraction\n")
       Nothing -> error ("Variable not declared - "++$1++"\n")
       Just (R recpost) -> recpost:($3 env)}

Llit2:: {[Lit]}
Llit2: lit { [$1] }
  | lit ',' Llit2 { $1:$3}


LInt::{[Int]}
LInt: intNum {[$1]}
  | intNum ',' LInt {$1:$3}


Formula: QFormula  {$1}
  | '(' Formula ')' {$2}
  | Formula '&&' Formula 
    { And [$1,$3] }
  | Formula '||' Formula 
    { Or [$1,$3] }
  | true { fTrue }
  | false { fFalse }


QFormula:: {Formula}
QFormula: LBExpr { let (f,rest)=$1 in f}
  | exists '(' LPorUSizeVar1 ':' Formula ')' 
    { fExists (reverse $3) $5 }
  | forall '(' LPorUSizeVar1 ':' Formula ')' 
    { fForall (reverse $3) $5 }
  | '!' '(' Formula ')' 
    { fNot $3 }

  
-- from the final result of qs, only Formula is useful
-- [[Update]] is needed only in the intermediate productions
LBExpr:: {(Formula,[[Update]])}
LBExpr:
  BExpr {$1}
  | lit '(' LPorUSizeVar1 ')' {(AppRecPost $1 (reverse $3),[])}
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

LPorUSizeVar: {- empty -} {[]}
  | LPorUSizeVar1 {$1}

LPorUSizeVar1: LPorUSizeVar1 ',' PorUSizeVar {$3:$1}
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
parse :: String -> Flags -> IO ()
parse s flags = 
  let listFunc = runP s parseCalc in
  let parseFuncFS = foldM (\env -> \func -> func env) emptyRelEnv listFunc in 
  runFS (initialState flags) parseFuncFS >>= \lastenv -> return ()

type RelEnv = [(Lit,Value)]

data Value = QF QFormula
  | F Formula
  | R RecPost

emptyRelEnv :: RelEnv
emptyRelEnv = []

extendRelEnv :: RelEnv -> (Lit,Value) -> RelEnv
extendRelEnv gamma (var,ty) = (var,ty):gamma

lookupVar :: Lit -> RelEnv -> Maybe Value
lookupVar lit [] = Nothing
lookupVar lit env@((v,f):rest) | (lit == v) = Just f
  | otherwise = lookupVar lit rest

groupDisjuncts:: [(Formula,Int)] -> [Int] -> [Formula] -> [Formula]
groupDisjuncts [] uniqueIds partialFormulae = partialFormulae
groupDisjuncts ((d,groupId):disj) uniqueIds partialFormulae =
  let ix = fromJust (elemIndex groupId uniqueIds) in
  let newPartialFormulae = updateList partialFormulae ix (Or [partialFormulae!!ix,d]) in
  groupDisjuncts disj uniqueIds newPartialFormulae
  
--updateList:: [a] -> (Int,a) -> [a]
--updateList xs (i,upd) = updateList1 xs (i,upd) 0
--  where
--  updateList1 xs (i,upd) j = 
--    if (i==j) then upd:(tail xs)
--    else updateList1 (tail xs) (i,upd) (j+1)
}
