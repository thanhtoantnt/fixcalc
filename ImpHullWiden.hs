{- |Provides operators for Hulling and Widening on the powerset domain of polyhedra -}
-----Operators common to BU and TD
module ImpHullWiden(
  closure,
  combHull,       -- |Given F in DNF-form, performs convex-hulling and returns a conjunctive formula.
  combSelHull,    -- |Given F in DNF-form and m, performs selective hulling. Ensures that length(res)=m. The third argument is not used.
  widen,          -- |Disjunctive widening. Given xs and ys, requires that length xs=length ys.
  narrow,
  widenOne,       -- |Conjunctive widening. 
  countDisjuncts, -- |Given F in DNF-form (e.g. result of simplify), returns the number of disjuncts from F.
  getDisjuncts,   -- |Given F in DNF-form (e.g. result of simplify), returns a list with the disjuncts from F.
  Disjunct,       -- |Conjunctive formula. The Or constructor is not used.
  DisjFormula     -- |Formula in DNF form equivalent to (Or [Formula]). The Or constructor is not used in any Formula in the list.
) where
import Fresh(FS,addOmegaStr,putStrFS,putStrNoLnFS,getLineFS,hFlushStdoutFS,getFlags,putStrFS_debug,putStrFS_DD,print_DD,print_RES)
import ImpAST
import System.IO.Unsafe(unsafePerformIO)
import ImpConfig(noExistentialsInDisjuncts,showDebugMSG,Heur(..),FixFlags)
import ImpFormula
  -- (simplify,hull,subset)
import MyPrelude(numsFrom,updateList,singleton,concatSepBy)
---------------
import Data.Array(Array,(//),(!),array,assocs,bounds)
import Data.Char(digitToInt,isDigit)
import Data.List(nub,union,(\\),sortBy,isInfixOf)
import Data.Maybe(catMaybes,fromJust)
import Control.Monad(filterM,when,foldM)

type Disjunct = Formula 
fFalseDisjunct :: Disjunct
fFalseDisjunct = EqK [Const 1]
type DisjFormula = [Formula] 

----------------------------------
--------Selective Hull------------
----------------------------------
combSelHull :: FixFlags -> DisjFormula -> [Formula] -> FS DisjFormula
-- requires: disj represents the DNF-form of a formula f (Or fs)
-- requires: m>=1
-- ensures: length(res)=m
combSelHull (m,heur) disj_in fbase_ls = 
  putStrFS_debug "CombSelHull!" >> 
  getFlags >>= \flags ->
  pairwiseCheck_disj disj_in >>= \disj ->
  addOmegaStr ("# SelhullIN:=" ++ showSet(Or disj)) >> 
  (if length disj <= m 
  then
     return disj
  else case m of
    1 -> 
      putStrFS_debug "CombSelHull! ==> m=1" >> 
      combHull fbase_ls disj >>= \h -> 
      return [h]
    _ -> -- assert (1<m<(length disj))
      mapM hullExistentials disj >>= \disjNoEx ->
      let disjM = map (\d -> Just d) disjNoEx in
      (putStrFS_DD 2 ("####combSelHull with "++show (length (catMaybes disjM))++ " disjuncts, --> "++(show m)
                                   ++ " disjuncts:\n" ++ concatSepBy "\n" (map (\mf -> case mf of {Nothing -> "Nothing";Just f -> showSet f}) disjM))) >>
      computeHalfMx heur fbase_ls disjM >>= \affinMx ->
      iterateHalfMx (m,heur) fbase_ls disjM affinMx >>= \relatedDisjM ->
      (putStrFS_DD 2 ("####combSelHull target :\n"
                      ++ concatSepBy "\n" (map (\mf -> case mf of {Nothing -> "Nothing";Just f -> showSet f}) relatedDisjM))) >>
      return (catMaybes relatedDisjM)
  ) >>= \res ->
  -- putStrFS ("Disj :"++(showSet (Or disj))) >>
  -- putStrFS ("Base :"++(show fbase)) >>
  -- putStrFS ("SHull :"++(showSet (Or res))) >>
  addOmegaStr("# SelhullOUT:=" ++ showSet(Or res)) >> 
  print_RES "combSelHull" (3) [("Disj(orig)",showSet (Or disj_in)),
                      ("Disj_exact",showSet (Or disj)),
                      ("Base",show fbase_ls),
                      ("SHull",showSet (Or res))
                      -- ,("closure",show fcrts)
                     ] >>
  return res

combHull :: [Formula] -> DisjFormula -> FS Formula
-- requires: disj represents the DNF-form of a formula f (Or fs)
combHull fbase_ls disj =
  putStrFS_debug ("combHull"++(show (Or disj))) >>
  combHull_x fbase_ls disj >>= \res ->
  print_RES "combHull" 100 [("fbase_ls",show fbase_ls),("ans",show res)] >>
  return res

combHull_x :: [Formula] -> DisjFormula -> FS Formula
-- requires: disj represents the DNF-form of a formula f (Or fs)
combHull_x fbase_ls disj =
  let dd = Or disj in
  putStrFS_debug ("before hull") >> 
  hull (dd) >>= \hulled ->
  putStrFS_debug ("after hull") >> 
  -- if fbase_ls==[] 
  -- then 
  --     putStrFS_debug ("combHull(res)"++(show hulled)) >>
  --   return hulled
  -- else
  keepProp fbase_ls dd >>= \ext_f ->
  let ans = And (hulled:ext_f) in
  return ans 

-- TODO WN 
keepProp:: [Formula] -> Formula -> FS [Formula]
-- requires: disj represents the DNF-form of a formula f (Or fs)
keepProp fbase orig = 
  let to_keep =  filter (isEqualF) (getConjunctsN orig) in
  mapM (subset orig) fbase >>= \suboks ->
  let fcrts' = zip fbase suboks in
  let fcrt' = filter (\(f,ok) -> ok) fcrts' in
  let ans = (map fst fcrt') in
  let kept = filter (isEqualF) ans in
  let ans2 =if kept==[] && length to_keep<2 then to_keep++ans else ans in
  print_RES "keepProp" (3) [("fbase",show fbase),
                      ("orig",show orig),
                      ("to_keep",show to_keep),
                      ("kept",show kept),
                      ("result",show ans2)
                     ] >>
  return ans2

computeHalfMx :: Heur -> [Formula] -> [Maybe Disjunct] -> FS AffinMx
-- ensures: (n,n)=length res, where n=length disj
computeHalfMx heur fbase_ls disj = 
  putStrFS_debug "computeHalfMx!" >> 
  let n = length disj-1 in 
  let mx = initAffinMx n in
  computeHalfMx1 heur mx (n,n) 0 disj
  where
      computeHalfMx1:: Heur -> AffinMx -> (Int,Int) -> Int -> [Maybe Disjunct] -> FS AffinMx
      computeHalfMx1 heur mat (m,n) i disj | i>n = return mat
      computeHalfMx1 heur mat (m,n) i disj = 
        computeHalfRow heur fbase_ls mat (m,n) i (i+1) disj >>= \mat1 ->
        putStrFS_debug "HalfMx 1!" >> 
        computeHalfMx1 heur mat1 (m,n) (i+1) disj


-- computes Affinities for second-half of row i, between columns j(first call uses i+1) and n
computeHalfRow:: Heur -> [Formula] -> AffinMx -> (Int,Int) -> Int -> Int -> [Maybe Disjunct] -> FS AffinMx
computeHalfRow heur fbase_ls mat (m,n) i j disj | j>n = return mat
computeHalfRow heur fbase_ls mat (m,n) i j disj = 
  affinity (disj!!i) (disj!!j) heur fbase_ls comb2Hull (nub $ concatMap fqsv (catMaybes disj))>>= \affinIJ -> 
  let newmat = mat // [((i,j),affinIJ)] in
  computeHalfRow heur fbase_ls newmat (m,n) i (j+1) disj
-- computes Affinities for upper-half of column j, between rows i(first call uses j-1) and 0
computeHalfCol:: Heur -> [Formula] -> AffinMx -> (Int,Int) -> Int -> Int -> [Maybe Disjunct] -> FS AffinMx
computeHalfCol heur fbase_ls mat (m,n) i j disj | i<0 = return mat
computeHalfCol heur fbase_ls mat (m,n) i j disj = 
  affinity (disj!!i) (disj!!j) heur fbase_ls comb2Hull (nub $ concatMap fqsv (catMaybes disj)) >>= \affinIJ -> 
  let newmat = mat // [((i,j),affinIJ)] in
  computeHalfCol heur fbase_ls newmat (m,n) (i-1) j disj

recomputeRow :: Heur -> [Formula] -> AffinMx -> Int -> [Maybe Disjunct] -> Int -> FS AffinMx
recomputeRow heur fbase_ls mat row disj dim =
  let r = [i | i <-[row+1..dim], not((disj!!i)==Nothing)] in
  foldM (\af -> \c -> 
                let qv = (nub $ concatMap fqsv (catMaybes [disj!!row,disj!!c])) in
                affinity (disj!!row) (disj!!c) heur fbase_ls comb2Hull qv >>= \ aff_new ->
                let newmat = mat // [((row,c),aff_new)] in
                return newmat) mat r 
  
mkZero:: AffinMx -> Int -> Int -> FS AffinMx
mkZero mat row dim = return (mat // ([((row,i),-1)|i<-[0..dim]]++[((i,row),-1)|i<-[0..dim]]))

-- computes Affinities for upper-half of column j, between rows i(first call uses j-1) and 0
computeHalfElems:: Heur -> [Formula] -> AffinMx -> [Maybe Disjunct] -> Int -> [Int] -> FS AffinMx
computeHalfElems heur fbase_ls mat _ dim [] = return mat
computeHalfElems heur fbase_ls mat replDisjM dim (i:ls) = 
  foldM (\mat -> \j -> mkZero mat j dim) mat ls  >>= \mat2 ->
  recomputeRow heur fbase_ls mat2 i replDisjM dim

-- computes Affinities for upper-half of column j, between rows i(first call uses j-1) and 0
computeHalfList :: Heur -> [Formula] -> AffinMx -> [Maybe Disjunct] -> Int -> [(Int,Int)] -> FS AffinMx
computeHalfList heur fbase_ls mat _ dim [] = return mat
computeHalfList heur fbase_ls affinMx replDisjM dim ((i,j):ls) = 
  putStrFS_DD 2 "!!computeHalfList" >> 
  mkZero affinMx j dim >>= \affinMx1 ->
  recomputeRow heur fbase_ls affinMx1 i replDisjM dim >>= \affinMx2 ->
  -- computeHalfRow heur affinMx (length replDisjM-1,length replDisjM-1) i (i+1) replDisjM >>= \affinMx1->
  -- computeHalfCol heur affinMx1 (length replDisjM-1,length replDisjM-1) (i-1) i replDisjM >>= \affinMx2->
  -- computeHalfRow heur affinMx2 (length replDisjM-1,length replDisjM-1) j (j+1) replDisjM >>= \affinMx3->
  -- computeHalfCol heur affinMx3 (length replDisjM-1,length replDisjM-1) (j-1) j replDisjM >>= \affinMx4->
  computeHalfList heur fbase_ls affinMx2 replDisjM dim ls

iterateHalfMx :: FixFlags -> [Formula] -> [Maybe Disjunct] -> AffinMx -> FS [Maybe Disjunct]
iterateHalfMx (m,heur) fbase_ls disjM affinMx = 
  putStrFS_DD 2 "!!iterateHalfMx" >> 
  getFlags >>= \flags -> 
  (putStrFS_DD 2 ("SelHullMatrix " ++ showAffinMx affinMx)) >>
  -- chooseElem heur affinMx >>= \(i,j) ->
  chooseAllMax disjM heur affinMx >>= \max_ls ->
  --putStrFS ("IterateHalfMx") >>
  let (dist_pairs,all_elems) = chooseDistElems max_ls in
  (putStrFS_DD 2 ("OrigMatrix :" ++ show (disjM))) >>
  -- (putStrFS_DD 2 ("Chosen elem is: " ++ show (i+1,j+1))) >>
  (putStrFS_DD 2 ("Chosen max elems are: " ++ show (norm_list max_ls))) >>
  (putStrFS_DD 2 ("Chosen dist_pairs are: " ++ show (norm_list dist_pairs))) >>
  (putStrFS_DD 2 ("Chosen all_elems are: " ++ show (norm_elem all_elems))) >>
  -- when (showDebugMSG flags >=1 && (affinMx!(i,j))<100) (putStrFS ("SelHull chose disjuncts with less than 100% affinity: "++ show (affinMx!(i,j)))) >>
  -- replaceRelated disjM (i,j) >>= \replDisjM ->
  replaceRelated_either fbase_ls disjM dist_pairs all_elems >>= \(replDisjM,hull_list,elm_list) ->
  (putStrFS_DD 2 ("List elems hulled: " ++ show (norm_list hull_list))) >>
  (putStrFS_DD 2 ("List elems merged: " ++ show (norm_elem elm_list))) >>
  let new_m = length (catMaybes replDisjM) in
  if new_m<=m then
    {- WN : to change m to a smaller value -}
    return replDisjM
  else
    -- (putStrFS_DD 2 ("####SelHull with "++show (length (catMaybes replDisjM))
    --     ++ " disjuncts:\n" ++ concatSepBy "\n" (map (\mf -> case mf of {Nothing -> "Nothing";Just f -> showSet f}) replDisjM))) >>
    let dim = length replDisjM-1 in
    computeHalfList heur fbase_ls affinMx replDisjM dim hull_list >>= \affinMx1 -> 
    computeHalfElems heur fbase_ls affinMx1 replDisjM dim elm_list >>= \affinMx4 -> 
    -- computeHalfRow heur affinMx (length replDisjM-1,length replDisjM-1) i (i+1) replDisjM >>= \affinMx1->
    -- computeHalfCol heur affinMx1 (length replDisjM-1,length replDisjM-1) (i-1) i replDisjM >>= \affinMx2->
    -- computeHalfRow heur affinMx2 (length replDisjM-1,length replDisjM-1) j (j+1) replDisjM >>= \affinMx3->
    -- computeHalfCol heur affinMx3 (length replDisjM-1,length replDisjM-1) (j-1) j replDisjM >>= \affinMx4->
    iterateHalfMx (m,heur) fbase_ls replDisjM affinMx4

-- replaces two related disjuncts with their hull
replaceRelated :: [Formula] -> [Maybe Disjunct] -> (Int,Int) -> FS [Maybe Disjunct]
-- requires: (0<=i,j<length disj)
-- ensures: length res=length disj
replaceRelated fbase_ls disj (i,j) =
  putStrFS_debug "replaceRelated" >> 
  let relI = map (\i -> fromJust (disj!!i)) [i,j] in
  combHull (fbase_ls) relI >>= \hulled ->
  -- putStrFS ("fbase_pair:="++(show fbase_ls)) >>
  -- putStrFS ("hull_pair:="++(show hulled)) >>
  let disjI = updateList disj i (Just hulled) in
  -- let disjI = updateList disj i Nothing in
  let disjIJ = updateList disjI j Nothing in
  print_DD True 3 [("replaceRelated",show disj),
                   ("fbase_ls",show fbase_ls),
                   ("to hull",show relI),
                   ("hulled",show hulled),
                   ("result",show disjIJ)] >>
  return disjIJ


replaceRelated_elems :: [Formula] -> [Maybe Disjunct] -> [Int] -> FS [Maybe Disjunct]
replaceRelated_elems fbase_ls disj (a:b:ls) = 
  let relI = map (\i -> fromJust (disj!!i)) (a:b:ls) in
  combHull (fbase_ls) relI >>= \hulled ->
  -- putStrFS ("fbase_elems:="++(show fbase)) >>
  -- putStrFS ("hull_elems:="++(show hulled)) >>
  let disjI = updateList disj a (Just hulled) in
  let disjIJ = zeroList disjI (b:ls) in
  return disjIJ 

zeroList disj [] = disj 
zeroList disj (b:ls) = 
  let disjI = updateList disj b Nothing in
  zeroList disjI ls

-- replaces pairs of related disjuncts with their hull
replaceRelated_list :: [Formula] -> [Maybe Disjunct] -> [(Int,Int)] -> FS [Maybe Disjunct]
-- requires: (0<=i,j<length disj)
-- ensures: length res=length disj
replaceRelated_list fbase disj ls =
  putStrFS_debug "replaceRelated_list" >> 
  helper disj ls 
  where
    helper disj [] = 
      return disj
    helper disj (p:ls) =
      replaceRelated fbase disj p >>= \new_disj ->
      helper new_disj ls

replaceRelated_either :: [Formula] -> [Maybe Disjunct] -> [(Int,Int)] -> [Int] -> FS ([Maybe Disjunct],[(Int,Int)],[Int])
-- requires: (0<=i,j<length disj)
-- ensures: length res=length disj
-- returns also a list of rows to be nullified
replaceRelated_either fbase disj ls elms = 
  putStrFS_debug "replaceRelated_either" >> 
  if elms==[] 
  then replaceRelated_list fbase disj ls >>= \ a -> return (a,ls,[])
  else replaceRelated_elems fbase disj elms >>= \ a -> return (a,[],elms)

comb2Hull :: Formula -> Formula -> FS Formula
comb2Hull f1 f2 = 
  hull (Or [f1,f2]) >>= \ans ->
  print_DD True (-8) [("com2Hull(F1)",show f1),
                      ("com2Hull(F2)",show f2),
                      ("com2Hull(ans)",show ans)
                      ] >>
  return ans

comb2Widen :: [Formula] -> Formula -> Formula -> FS Formula
comb2Widen fbase_ls f1 f2 = 
  widenOne fbase_ls (f1,f2) >>= \ ans ->
  print_DD True (-8) [("widenOne(F1)",show f1),
                      ("widenOne(F2)",show f2),
                      ("widenOne(ans)",show ans)
                      ] >>
  return ans
  
-- WN to fix
moreSelHull xs ys heur =
  putStrFS_debug "moreSelHull(widen)" >> 
  helper xs ys
  where
    helper xs ys =
      let x_len = length xs in
      let y_len = length ys in
      when (not (x_len == y_len)) 
          (putStrFS_DD 2 ("WARNING: hulling applied to widening two formula of different disjuncts\n"
                         ++showSet (Or xs) ++ "\n" ++ showSet(Or ys))) >>
      if x_len==y_len 
      then return (xs,ys)
      else
        if x_len<y_len 
        then
          combSelHull (x_len,heur) ys [] >>= \new_ys ->
          helper xs new_ys
        else
          combSelHull (y_len,heur) xs [] >>= \new_xs ->
          helper new_xs ys 

narrow :: Heur -> [Formula] -> (DisjFormula,DisjFormula) -> FS DisjFormula
narrow heur fbase_ls (xs,ys) =
  let xs_f = Or xs in
  let ys_f = Or ys in
  complement xs_f >>= \xs_compl ->
  pairwiseCheck xs_compl >>= \xs_pwc ->
  complement ys_f >>= \ys_compl ->
  pairwiseCheck ys_compl >>= \ys_pwc ->
  widen heur fbase_ls (getDisjuncts xs_pwc, getDisjuncts ys_pwc) >>= \widen_result ->
  complement (Or widen_result) >>= \result ->
  return (getDisjuncts result)
  
checkConjunct :: [Formula] -> Disjunct -> Bool
checkConjunct conjuncts disjunct =
  case conjuncts of
    [conjunct] -> isInfixOf (show conjunct) (show disjunct)
    x:xs -> (isInfixOf (show x) (show disjunct)) && (checkConjunct  xs disjunct)
  
narrowOne :: [Formula] -> (Disjunct,Disjunct) -> FS Disjunct
              -- requires: fcrt, fnext are conjunctive formulae
narrowOne fbase_ls (fcrt,fnext) = 
  addOmegaStr ("NarrowCrt:=" ++ showSet fcrt) >> 
  addOmegaStr("NarrowNxt:=" ++ showSet fnext) >>
  saturateFS fcrt >>= \satf ->    -- 
  let satf_l = getConjunctsN satf in
  -- closure fcrt >>= \fcrts ->    --
  let new_ls = (satf_l++fbase_ls) in
  keepProp new_ls fnext >>= \implied_ls ->
  let fwid = fAnd (implied_ls) in
  print_RES "narrowOne" (3) [("fcrt",show fcrt),
                      ("fnext",show fnext),
                      ("fbase_ls",show fbase_ls),
                      ("Sat(fcrt)",show satf_l),
                      ("result",show fwid)
                      -- ,("closure",show fcrts)
                     ] >>
  
  subset fcrt fnext >>= \bool1 -> 
  subset fnext fcrt >>= \bool2 ->
  let result = (if (bool1 && bool2) then fwid else fFalse) in
  return result

----------------------------------
--------Widening powersets--------
----------------------------------
widen :: Heur -> [Formula] -> (DisjFormula,DisjFormula) -> FS DisjFormula
-- requires (length xs)=(length ys)
-- ensures (length res)=(length xs)
widen heur fbase_ls (xs,ys) =
  -- let fbase_ls = getConjunctsN fbase in
  putStrFS_debug "widen!" >> 
  getFlags >>= \flags ->
  -- let x_len = length xs in
  -- let y_len = length ys in
  pairwiseCheck (Or xs) >>= \xs_pwc ->
  pairwiseCheck (Or ys) >>= \ys_pwc ->
  let xs = getDisjuncts xs_pwc in
  let ys = getDisjuncts ys_pwc in
  moreSelHull xs ys heur >>= \ (xs,ys) ->
  mapM hullExistentials xs >>= \xsNoEx ->
  mapM hullExistentials ys >>= \ysNoEx ->
  addOmegaStr ("Widen1IN:=" ++ showSet(Or xsNoEx)) >> 
  addOmegaStr ("Widen2IN:=" ++ showSet(Or ysNoEx)) >> 
  -- let (mxs,mys) = (map (\x -> Just x) xsNoEx,map (\y -> Just y) ysNoEx) in
  let (mxs,mys) = (xsNoEx,ysNoEx) in
  computeMx_full heur fbase_ls (mxs,mys) >>= \affinMx ->
  iterateMx_full heur fbase_ls (mxs,mys) affinMx [] >>= \ijs ->
  mapM (\(i,j) -> widenOne fbase_ls (xsNoEx!!i,ysNoEx!!j)) ijs >>= \res ->
  -- WN :causing LOOP?
  let ans = Or res in
  print_DD True 2 [("widen(affixMx)",show affinMx),("widen(list)",show ijs),("widen(res)",show ans)] >>
  addOmegaStr ("WidenOUT:=" ++ showSet(ans)) >> 
  return res
  
computeMx_full :: Heur -> [Formula] -> ([Disjunct],[Disjunct]) -> FS AffinMx
-- requires: length disjCrt = length disjNxt
computeMx_full heur fbase_ls (disjCrt,disjNxt) =
  putStrFS_debug "computeMx_full" >>
  let n = length disjCrt-1 in 
  let mx = initAffinMx n in
  computeMx1 heur fbase_ls mx (n,n) 0 (disjCrt,disjNxt) >>= \r ->
  putStrFS_debug ("computeMx_full(ans):"++(show r)) >>
  return r
  where
      computeMx1:: Heur -> [Formula] -> AffinMx -> (Int,Int) -> Int -> ([Disjunct],[Disjunct]) -> FS AffinMx
      computeMx1 heur fbase_ls mat (m,n) i (disjCrt,disjNxt) | i>n = return mat
      computeMx1 heur fbase_ls mat (m,n) i (disjCrt,disjNxt) = 
        computeRow heur fbase_ls mat (m,n) i 0 (disjCrt,disjNxt) >>= \mat1 ->
        computeMx1 heur fbase_ls mat1 (m,n) (i+1) (disjCrt,disjNxt)

-- computes Affinities for row i
computeRow:: Heur -> [Formula] -> AffinMx -> (Int,Int) -> Int -> Int -> ([Disjunct],[Disjunct]) -> FS AffinMx
computeRow heur fbase_ls mat (m,n) i j (disjCrt,disjNxt) | j>n = return mat
computeRow heur fbase_ls mat (m,n) i j (disjCrt,disjNxt) = 
  affinity (Just (disjCrt!!i)) (Just (disjNxt!!j)) heur fbase_ls (comb2Widen fbase_ls) (nub $ concatMap fqsv ((disjCrt++disjNxt))) >>= \affinIJ -> 
  let newmat = mat // [((i,j),affinIJ)] in
  computeRow heur fbase_ls newmat (m,n) i (j+1) (disjCrt,disjNxt)

-- computes Affinities for col j
computeCol :: Heur -> [Formula] -> AffinMx -> (Int,Int) -> Int -> Int -> ([Disjunct],[Disjunct]) -> FS AffinMx
computeCol heur fbase_ls mat (m,n) i j (disjCrt,disjNxt) | i>n = return mat
computeCol heur fbase_ls mat (m,n) i j (disjCrt,disjNxt) = 
  affinity (Just (disjCrt!!i)) (Just (disjNxt!!j)) heur fbase_ls (comb2Widen fbase_ls) (nub $ concatMap fqsv ((disjCrt++disjNxt))) >>= \affinIJ -> 
  let newmat = mat // [((i,j),affinIJ)] in
  computeCol heur fbase_ls newmat (m,n) (i+1) j (disjCrt,disjNxt)

-- called by widening!...
iterateMx_full :: Heur -> [Formula] -> ([Disjunct],[Disjunct]) -> AffinMx -> [(Int,Int)] -> FS [(Int,Int)]
iterateMx_full heur fbase_ls (disjCrt,disjNxt) affinMx partIJs = 
  putStrFS_debug "iterateMx_full" >> 
  getFlags >>= \flags -> 
  (putStrFS_DD 2 ("####Widening 2 arguments, each with "++show (length (disjCrt)) 
            ++ " disjuncts:\n" ++ concatSepBy "\n" (map (\mf -> showSet mf) (disjCrt++disjNxt)))) >>
  (putStrFS_DD 1 ("WidenMatrix "++showAffinMx affinMx)) >>
  chooseElem disjCrt disjNxt heur affinMx >>= \(i,j) ->
  -- chooseAllMax heur affinMx >>= \max_ls ->
  -- let (dist_pairs,all_elems) = chooseDistElems max_ls in  
  (putStrFS_DD 1 ("Chosen elem is: " ++ show (i+1,j+1))) >>
  let mat_len = length disjCrt in
  let new_partIJs = (i,j):partIJs in
  if length new_partIJs == mat_len then
    return new_partIJs
  else
    let new_affixMx = removeAffMx mat_len affinMx i j in
    iterateMx_full heur fbase_ls (disjCrt,disjNxt) new_affixMx new_partIJs 
  -- (putStrFS_DD 1 ("Chosen max elems are: " ++ show (norm_list max_ls))) >>
  -- (putStrFS_DD 2 ("Chosen dist_pairs are: " ++ show (norm_list dist_pairs))) >>
  -- (putStrFS_DD 2 ("Chosen all_elems are: " ++ show (norm_elem all_elems))) >>
  -- replaceRelatedWithNoth (Just disjCrt,Just disjNxt) (i,j) >>= \(replDisjCrt,replDisjNxt) ->
  -- if (length (catMaybes replDisjCrt))==0 then return ((i,j):partIJs)
  -- else 
  --   computeRow heur fbase_ls affinMx (length replDisjCrt-1,length replDisjCrt-1) i 0 (replDisjCrt,replDisjNxt) >>= \affinMx1->
  --   computeCol heur fbase_ls affinMx1 (length replDisjCrt-1,length replDisjCrt-1) 0 j (replDisjCrt,replDisjNxt) >>= \affinMx2->
  --   iterateMx_full heur fbase_ls (replDisjCrt,replDisjNxt) affinMx2 ((i,j):partIJs)

removeAffMx :: Int -> AffinMx -> Int -> Int -> AffinMx
removeAffMx dim a i j = 
  let ls = [0..(dim-1)] in
  let rows = [((i,k),identityA)|k<-ls] in
  let cols = [((k,j),identityA)|k<-ls] in
  a // (rows++cols)

-- replaces two related disjuncts with Nothing
replaceRelatedWithNoth:: ([Maybe Disjunct],[Maybe Disjunct]) -> (Int,Int) -> FS ([Maybe Disjunct],[Maybe Disjunct])
replaceRelatedWithNoth (disjCrt,disjNxt) (i,j) =
  let disjI = updateList disjCrt i Nothing in
  let disjJ = updateList disjNxt j Nothing in
  return (disjI,disjJ)


----------------------------------
--------Widening on conj domain---
----------------------------------
widenOne :: [Formula] -> (Disjunct,Disjunct) -> FS Disjunct
-- requires: fcrt, fnext are conjunctive formulae
widenOne fbase_ls (fcrt,fnext) = 
  putStrFS_DD 1 ("fcrt: " ++ showSet fcrt) >>
  putStrFS_DD 1 ("fnext: " ++ showSet fnext) >>
  addOmegaStr ("WidenCrt:=" ++ showSet fcrt) >> 
  -- WN : cause LOOP?
  addOmegaStr("WidenNxt:=" ++ showSet fnext) >>
  saturateFS fcrt >>= \satf ->    -- 
  let satf_l = getConjunctsN satf in
  -- closure fcrt >>= \fcrts ->    --
  let new_ls = (satf_l++fbase_ls) in
  keepProp new_ls fnext >>= \implied_ls ->
  let fwid = fAnd (implied_ls) in
  print_RES "widenOne" (3) [("fcrt",show fcrt),
                      ("fnext",show fnext),
                      ("fbase_ls",show fbase_ls),
                      ("Sat(fcrt)",show satf_l),
                      ("result",show fwid)
                      -- ,("closure",show fcrts)
                     ] >>
  addOmegaStr ("WidenRes:=" ++ showSet fwid) >>
  -- print_DD True 3 [("fbase_ls",show fbase_ls),("widenOne(before)",show fcrt),("widenOne(next)",show fnext)] >>
  -- print_DD True 3 [("widenOne(result)",show fwid)]  >>
  return fwid

closure:: Disjunct -> FS [Disjunct]
-- requires: f is conjunctive formula
closure f =
  let updSubst = [] in
  let conjs = buildClauses updSubst f in
  let noconst = discoverIneqWithoutNegConstant conjs in
  discover2Ineq conjs >>= \discov ->
  let ans = conjs++discov++noconst in
  putStrFS_DD (-13) ("inp:"++(show f)) >>
  putStrFS_DD (-13) ("closure:"++(show ans)) >>
  return (ans)
  
  where
    -- input: (i+13<=j)
    -- output: (i<=j)
    discoverIneqWithoutNegConstant:: [Disjunct] -> [Disjunct]
    -- requires: formula is in conjunctive form
    discoverIneqWithoutNegConstant fs = 
      let newfs = map discoverIneqWithoutNegConstant1 fs in
      (nub newfs) \\ fs
    discoverIneqWithoutNegConstant1:: Disjunct -> Disjunct
    discoverIneqWithoutNegConstant1 formula = case formula of
      And fs -> fAnd (map discoverIneqWithoutNegConstant1 fs)
      GEq us -> let newus = filter (\u -> case u of {Const x -> if x<0 then False else True; Coef _ _ -> True}) us in
                GEq newus
      EqK us -> formula
      _ -> error ("unexpected argument: "++show formula)
    
    -- input: (i<=j && 4a<=2+i+3j)
    -- output: (a<=j)
    discover2Ineq:: [Disjunct] -> FS [Disjunct]
    discover2Ineq fs =
      let vars = fqsv (fAnd fs) in
      let singletons = map (\x -> [x]) vars in
      let pairs = genPairs vars in
      mapM (discover2Relation fs vars) (pairs) >>= \newfs ->
      let filtfs = filter (\f -> formulaIsNotEqK f) (nub $ concat newfs) in
      return (filtfs \\ fs)
    discover2Relation:: [Disjunct] -> [QSizeVar] -> [QSizeVar] -> FS [Disjunct]
    discover2Relation fs allvars varsToKeep = hull (fExists (allvars \\ varsToKeep) (fAnd fs)) >>= \fsimpl ->
      return (formulaToConjList fsimpl)
    genPairs:: [a] -> [[a]]
    genPairs xs | length xs <=1 = []
    genPairs (x:xs) = 
      let p1 = map (\y -> [x,y]) xs in
      let p2 = genPairs xs in p1++p2
    formulaToConjList:: Disjunct -> [Disjunct]
    -- requires: formula is in conjunctive form
    formulaToConjList formula = case formula of
      And fs -> concatMap formulaToConjList fs
      GEq us -> [formula]
      EqK us -> [formula]
      _ -> error ("unexpected argument: "++show formula)
    formulaIsNotEqK formula = case formula of
      EqK us -> False
      _ -> True

    buildClauses:: [(QSizeVar,[Update])] -> Disjunct -> [Disjunct]
    buildClauses updSubst f = 
     case f of
       And fs -> concatMap (buildClauses updSubst) fs
       GEq ups -> f:(applyUpdSubst updSubst f)
       EqK ups -> -- [f]
         -- more precise widening if (f2-f1=1) is transformed to (1<=f2-f1 && f2-f1<=1)
         [GEq ups,GEq (map (mulUpdate (-1)) ups)] 
       _ -> error $ "widenOne: argument must be in conjunctive form\n " ++ show f
    -- input: (f1-f3>=0 && f1+f2=0)
    -- output: [(f1,[-f2]),(f2,[-f1])]
    collectUpdSubst:: Disjunct -> [(QSizeVar,[Update])]
    collectUpdSubst f =
     case f of
       And fs -> concatMap collectUpdSubst fs
       GEq ups -> []
       EqK ups -> 
         let obtainSubst = (\ups -> \u -> case u of {
                                                Const i -> [];
                                                Coef qsv 1 -> [(qsv,map (mulUpdate (-1)) (ups\\[u]))]; --  [(Coef qsv (-i),ups\\[u])]
                                                Coef qsv (-1) -> [(qsv,(ups\\[u]))];
                                                _ -> []}
                            ) in
         concatMap (obtainSubst ups) ups
       _ -> error $ "widenOne: argument must be in conjunctive form\n " ++ show f
    -- input: [(f1,[-f2]),(f2,[-f1])]
    -- input: (f1-f3>=0 && f1+f2=0)
    -- output: (f1-f3>=0 && -f2-f3>=0 && f1+f2=0)
    applyUpdSubst:: [(QSizeVar,[Update])] -> Disjunct -> [Disjunct]
    applyUpdSubst subs geq@(GEq _) = catMaybes $ map (\s -> applyOneUpdSubst s geq) subs
    applyOneUpdSubst:: (QSizeVar,[Update]) -> Disjunct -> Maybe Disjunct
    applyOneUpdSubst (qsv,ups) (GEq geqs) =
     let qsvIsIn = any (\u -> case u of {Coef qsv1 i -> qsv==qsv1; _ -> False}) geqs in 
     if qsvIsIn then
       let upsAfterSubs = concatMap (\u -> case u of {Coef qsv1 i -> if (qsv1==qsv) then map (mulUpdate i) ups else [u];_ -> [u]}) geqs in
       Just (GEq upsAfterSubs)
     else Nothing
    
    mulUpdate:: Int -> Update -> Update
    mulUpdate x (Const i) = Const (i*x)
    mulUpdate x (Coef qsv i) = Coef qsv (i*x)

----------------------------------
--------Affinity Matrix-----------
----------------------------------
type AffinMx = Array (Int,Int) Int
identityA = -1 
-- identityA should be smaller than all elements from AffinMx (so that "chooseElem" computes maximum element from AffinMx matrix)

initAffinMx:: Int -> AffinMx
initAffinMx n =
  let gen = take ((n+1)*(n+1)) (numsFrom 0) in
  let l = map (\x -> ((x `quot` (n+1),x `rem` (n+1)),identityA)) gen in
    array ((0,0),(n,n)) l

norm_pair:: (Int,Int) -> (Int,Int)
norm_pair (i,j) = (i+1,j+1)

rev_norm_pair:: (Int,Int) -> (Int,Int)
rev_norm_pair (i,j) = (i-1,j-1)

norm_list:: [(Int,Int)] -> [(Int,Int)]
norm_list ls = map norm_pair ls

rev_norm_list:: [(Int,Int)] -> [(Int,Int)]
rev_norm_list ls = map rev_norm_pair ls

norm_elem:: [Int] -> [Int]
norm_elem ls = map (\x -> x+1) ls

-- |Returns the indices of either the maximum element in the matrix or chosen by the user with SimInteractiveHeur.
chooseElem :: [Disjunct] -> [Disjunct] -> Heur -> AffinMx -> FS (Int,Int)
chooseElem disjMcurr disjMnext heur mat = 
  let firstMax = ((0,0),(mat!(0,0),0)) in
  let maxe = foldl (\((mi,mj),(amax,npos)) -> \((i,j),val) -> 
                     let nn = if val>=0 then npos+1 else npos in
                     if val>=amax 
                     then ((i,j),(val,nn)) 
                     else ((mi,mj),(amax,nn))) firstMax (assocs mat) in
  let (s_pair,(aff,npos))=maxe in
  case heur of
    SimInteractiveHeur ->
      putStrFS ("Widen(Curr):\n"++ showNumForm2 disjMcurr) >>
      putStrFS ("Widen(Next):\n"++ showNumForm2 disjMnext) >>
      putStrFS ("Affin Matrix:\n"++ showAffinMx mat) >>
      -- putStrFS ("maxe:"++show maxe) >>
      putStrFS ("MAX matching pair is: " ++ show ( fst (fst maxe)+1,snd (fst maxe)+1 )) >>
      if aff>=100 || npos<=1 
      then 
        putStrFS ("Matching pair automatically chosen!") >> hFlushStdoutFS >> 
        return (s_pair)
      else
        putStrNoLnFS ("Choose an elem: ") >> hFlushStdoutFS >> getLineFS >>= \str -> 
        stringToArray str >>= \ans ->
        case ans of
          [] ->
            return (s_pair)
             --return (getIndices str (fst maxe))
          b:_ ->
            return b
    _ -> 
      return (s_pair)
  
stringToArray :: String -> FS [(Int,Int)]
stringToArray s =
  helper s >>= \ans ->
  return (rev_norm_list ans)
  where 
    helper s =
      case s of
        "" -> 
          putStrFS ("You choice: NULL") >>  
          return []
        _  -> 
          foldM (\(c,d,final) -> \si ->
                  if (isDigit si) 
                  then return (si:c,d,final) 
                  else 
                    if si==',' 
                    then return ("",c,final)
                    else 
                      if si==')' 
                      then return ("","",(read d,read c):final) 
                      else 
                        return (c,d,final))  ("","",[]) s >>= \ar ->
          let (_,_,t)=ar in
          let v=reverse t in
          putStrFS ("Your Choice:"++(show v)) >>
          --mapM (\(x,y) ->  putStrFS ("Bach" ++ show x ++ show y )) t >>
          --return t
          return t



{-stringToArray1 :: String -> FS [(Int,Int)]
stringToArray1 s =
   case s of
        "" -> 
          return []
        _  -> 
          foldM (\(c,d,final) si -> 
                  return (c,d,final) ) ("","",[]) s >>= \a ->
          putStrFS ("maxe:"++show a) >> 
          let (_,_,t)=a in
          return t
-}

-- |Returns all maximum elements in the matrix or chosen by the user with SimInteractiveHeur.
chooseAllMax :: [Maybe Disjunct] -> Heur -> AffinMx -> FS [(Int,Int)]
chooseAllMax disjM heur mat = 
  let firstMax = ([],0) in
  let maxe = foldl (\(curr_ls,amax) -> \((i,j),val) -> if val>=amax then (if val>amax then ([(i,j)],val) else (([(i,j)]++curr_ls),val)) else (curr_ls,amax)) firstMax (assocs mat) in
  case heur of
    SimInteractiveHeur ->
      putStrFS ("Formulae\n"++ showNumForm disjM) >>
      putStrFS ("Affin Matrix\n"++ showAffinMx mat) >>
      let ls = norm_list (fst maxe) in
      putStrFS ("MAX elems: " ++ show ls) >>
      putStrNoLnFS ("Choose sublist of elems: ") >> 
      hFlushStdoutFS >> 
      getLineFS >>= \str ->
      -- return [(getIndices str (head ls))]
      stringToArray str >>= \a ->
        if((length a) == 0) then
          return (fst maxe)
        else 
          return a
    _ -> 
      return (fst maxe)

-- choose only distinct pairs of elements
chooseDist:: [(Int,Int)] -> [(Int,Int)]
chooseDist ls =
  snd (foldl (\(elems,ls) -> \(i,j) -> if member (i,j) elems 
                                 then (elems,ls) else ([i,j] `union` elems,[(i,j)]++ls) ) ([],[]) ls)
  where
    member (i,j) elems = (i `elem` elems) || (j `elem` elems)

-- choose distinct pairs or all the elements if strongly connected 
chooseDistElems:: [(Int,Int)] -> ([(Int,Int)],[Int])
chooseDistElems ls =
  let ls_len = length ls in
  if ls_len<=1 then (ls,[])
  else let dist_elems = foldl (\dl -> \(i,j) -> [i,j] `union` dl) [] ls 
       in let dist_len = length dist_elems in 
       if ls_len == dist_len 
       then ([],dist_elems)
       else (chooseDist ls,[])


getIndices:: String -> (Int,Int) -> (Int,Int)
getIndices str givenmax = 
  if length str >= 5 && str!!0 == '(' && isDigit (str!!1) && str!!2 == ',' && isDigit (str!!3) && str!!4 == ')' then
    (digitToInt (str!!1)-1, digitToInt (str!!3)-1)
  else givenmax

showNumForm:: [Maybe Formula] -> String
showNumForm s = 
  helper s 1
  where              
    helper [] n = ""
    helper (x:xs) n =
      (show n)++":"++(case x of {Just f -> show f; _ -> "None"})++
        (if xs==[] then "" else "\n")
        ++(helper xs (n+1))

showNumForm2 s = showNumForm ((map (\s->Just s)) s)

showAffinMx:: AffinMx -> String
showAffinMx mat = 
  let ((_,_),(m,n)) = bounds mat in 
    -- ("- noRows: "++show (m+1) ++ ", noCols: "++show (n+1)++"\n") ++  
    showMatrix mat (m,n) 0
  where
    showMatrix:: AffinMx -> (Int,Int) -> Int -> String
    showMatrix mat (m,n) i | i==m = showRow mat (m,n) i 0
    showMatrix mat (m,n) i = showRow mat (m,n) i 0 ++ "\n" ++ showMatrix mat (m,n) (i+1)
    showRow:: AffinMx -> (Int,Int) -> Int -> Int -> String
    showRow mat (m,n) i j | j>n = ""
    showRow mat (m,n) i j = show (mat!(i,j)) ++ " " ++ showRow mat (m,n) i (j+1)

-- remove Eq ctr from the list
removeEQ f1 = 
  filter (\e -> case e of
             EqK _ -> False
             _ -> True
         ) f1

remove_uselessFS :: [Formula] -> FS [Formula]
remove_uselessFS f1 =
  -- let f2 = removeEQ f1 in
  let ans = filter (\e -> 
           case e of
             -- EqK _ -> False
             EqK [Const _] -> False
             GEq [Const e] -> False
             _ -> True
         ) f1 in
  print_DD True (-9) [("before",show f1),("after useless",show ans)] >>
  return ans

elim_dupl fs =
  let nl = map (\e -> (e,show e)) fs in
  let sl = sortBy (\(_,e1) (_,e2) -> compare e1 e2) nl in
  helper sl 
  where
    helper [] = []
    helper [(a,_)] = [a]
    helper ((a1,e1):(ls@((a2,e2):xs))) = 
      case compare e1 e2 of
           EQ -> helper ls
           _ -> a1:(helper ls)
 
{- 
    pick equality from foperation to give higher weight
    add inequality contr from fbase_ls
    remove equality & true ctr from f1 and f2
-}
merge_set :: [Formula] -> Formula -> Formula -> Formula -> FS ([Formula],[Formula],Int)
      -- requires: f1,f2 are conjunctive formulae
merge_set fbase_ls f1 f2 foperation =
        putStrFS_debug "merge_set" >>
        let (conjf1,conjf2) = (getConjuncts f1,getConjuncts f2) in
        remove_uselessFS conjf1 >>= \ conjf1x ->
        remove_uselessFS conjf2 >>= \ conjf2x ->
        let conjf1y = elim_dupl conjf1x in
        let conjf2y = elim_dupl conjf2x in
        let combined_set = (conjf1y `union` conjf2y) in
        let n = length combined_set in
        filterM (\f -> subset foperation f) combined_set >>= \ans_orig ->
        filterM (\f -> subset foperation f) fbase_ls >>= \ans_fbase ->
        print_DD True (-7) [("f1",show f1),("f2",show f2),
                            ("conjf1x",show conjf1x),("conjf2x",show conjf2x),
                            ("conjf1y(no dupl)",show conjf1y),("conjf2y(no dupl)",show conjf2y),
                            ("hulled",show foperation)
                            ,("combined_set",show combined_set)
                            ,("implied_set(fbase)",show ans_fbase)
                            ,("implied_set(orig)",show ans_orig)
                            ] >>
        return (ans_orig,ans_fbase,n)

affinity :: Maybe Formula -> Maybe Formula -> Heur -> [Formula] -> (Formula -> Formula -> FS Formula) -> [QSizeVar] -> FS Int
-- requires: f1,f2 represent conjunctive formulae
-- TODO WN : should give higher weightage to infinity 
affinity f1 f2 heur fbase_ls operation fsv =
     putStrFS_debug "affinity!" >> 
  helper f1 f2 heur
  where
    helper Nothing _ _ =    return identityA
    helper _ Nothing _ =    return identityA
    -- affinity Nothing _ heur _ _ = return identityA
    -- affinity _ Nothing heur _ _ = return identityA
    helper (Just f1) (Just f2) HausdorffHeur =
        mapM (\x -> projectQSV f1 x) fsv >>= \ranges1 ->
        mapM (\x -> projectQSV f2 x) fsv >>= \ranges2 ->
        let distances = map hausdorffDistance (zip ranges1 ranges2) in
        let (inc,dist) = addHDistances distances in
        let maxdist = 1000 in
        let haus = ceiling (fromIntegral (100*inc) / fromIntegral (length fsv+1)) + 
                   ceiling (fromIntegral (100*dist) / fromIntegral ((length fsv+1)*maxdist))in
        --  putStrFS (concatMap show fsv) >>
        --  putStrFS ("haus: " ++ show (length fsv) ++ ":" ++ show inc ++ ":" ++ show dist ++ ":" ++ show haus ++ ":" ++ show (100-haus)) >>
        return (100-haus)
    helper (Just f1) (Just f2) heur = 
        operation f1 f2 >>= \foperation -> 
        let f_or = Or [f1,f2] in
        getFlags >>= \flags ->
        -- simplify foperation >>= \foperation ->
        -- simplify f_or >>= \f_or ->
        -- subset foperation f_or >>= \imp1 ->
        -- subset f_or foperation >>= \imp2 ->
        simplify (And [foperation,fNot(Or [f1,f2])]) >>= \fDif ->
        subset fDif fFalse >>= \difIsFalse ->
        if difIsFalse {-imp1 && imp2-} 
        then
            (putStrFS_DD 2("Full Match 100!")) >> 
            (putStrFS_DD 2("F1:="++showSet f1)) >> 
            (putStrFS_DD 2("F2:="++showSet f2)) >>
            (putStrFS_DD 2("foper:="++showSet foperation)) >>
            (putStrFS_DD 2("f_or:="++showSet f_or)) >>
            (putStrFS_DD 2("fDif:="++showSet fDif)) >>
            subset f1 f2 >>= \fb1 ->
            subset f2 f1 >>= \fb2 ->
            let v1 = if fb1 then 50 else 0 in
            let v2 = if fb2 then 50 else 0 in
            return (100+v1+v2)
        else
            subset fTrue foperation >>= \operationIsTrue ->
            if operationIsTrue 
            then return 0 
            else 
                case heur of
                  DifferenceHeur -> 
                    let n = countDisjuncts fDif in
                    let nsteps = if n>4 then 4 else n in
                    let disj = getDisjuncts fDif in
                    let getAverageConjuncts = (\c -> fromIntegral (countConjuncts c) / (fromIntegral n)) in
                    let s = ceiling $ sum (map getAverageConjuncts disj) in
                    let diffSteps = 100 - (20*nsteps-s) in
                    return diffSteps
                  _ -> {- SimilarityHeur, SimInteractiveHeur -}
                    (putStrFS_DD 2("F1:="++showSet f1)) >> 
                    (putStrFS_DD 2("F2:="++showSet f2)) >>
                    let (cf1,cf2) = (countConjuncts f1,countConjuncts f2) in
                    merge_set fbase_ls f1 f2 foperation >>= \(mSet,mSetFB,num_of_orig) ->   
                    let cmset = length mSet in
                    let cmsetFB = length mSetFB in
                          -- let n = length mSetFB in
                          -- if num_of_orig == 0 then n*3
                          -- else n in
                    let fblen = length fbase_ls in
                    -- let rr = if fblen == 0 then cmset else 2*cmset+cmsetFB in
                    let rr = cmset+cmsetFB in
                    let frac = (((fromIntegral (rr) / (fromIntegral (num_of_orig+fblen{- cf1+cf2 -}
                                                           )))*98)+1) in
                    when (frac>=100) (putStrFS_DD 0 ("WARNING frac >100: "++(show frac))) >>
                    (putStrFS_DD 2("cf1:="++show cf1 ++" cf2:="++show cf2++" cmset:="++show cmset)) >> 
                    (putStrFS_DD 2("Foper:="++showSet foperation)) >>
                    (putStrFS_DD 2("mSet::="++concatMap (\f -> showSet f) mSet)) >>
                    (putStrFS_DD 2("affin:="++show (cmset+cmsetFB) ++ "/" ++ show (num_of_orig+fblen) ++ "  " ++ show (ceiling frac))) >>
                    print_DD True 2 [("cmset",(show cmset)++" cmsetFB:"++(show cmsetFB)),
                                     ("num of orig",(show num_of_orig)++" fblen:"++ (show fblen))
                                     ] >>
                    return (ceiling frac)
    -- where
    --   mset:: Formula -> Formula -> Formula -> FS [Formula]
    --   -- requires: f1,f2 are conjunctive formulae
    --   mset f1 f2 foperation =
    --     let (conjf1,conjf2) = (getConjuncts f1,getConjuncts f2) in
    --     filterM (\f -> subset foperation f) (conjf1 `union` conjf2)

type Range = (Maybe Int,Maybe Int) 
-- ^A 'Range' value represents an interval: 'Nothing' means Infinity, 'Just' i means the integer i.
-- For example, (Nothing,Just 3) = (-inf,3]
projectQSV:: Formula -> QSizeVar -> FS Range
-- ^'projectQSV' computes from a formula, the range for some qsv.
-- For example, from (x+7>=0 && y>=0 && -x>=0) the range for x is [-7,0]. 
-- requires: f1 is in conjunctive form, without quantifiers.
-- requires: f1 contains at most 2 conjuncts (one upper bound and one lower bound).
projectQSV f1 qsv = 
  let f2 = fExists (fqsv f1 \\ [qsv]) f1 in
  hull f2 >>= \f3 -> 
--  putStrFS ("simpl:= " ++ show f3 ++ "\trange: " ++ show (extractRange f3)) >>
  return (extractRange f3)

extractRange:: Formula -> Range
extractRange formula = case formula of 
  And fs -> intersectRanges (map extractRange fs)
  GEq us -> 
    let coefVars = catMaybes $ map (\u -> case u of {Const _ -> Nothing;Coef _ i -> Just i}) us in
    let sumConsts = sum $ map (\u -> case u of {Const i -> i;Coef _ _ -> 0}) us in
    if (singleton coefVars) then
      let coef = head coefVars in
      case coef of 
        1 -> (Just (-sumConsts), Nothing)
        -1 -> (Nothing,Just sumConsts)
        _ -> error ("extractRange: unexepcted coefficient: "++show formula)
    else error ("extractRange: unexepcted coefficient: "++show formula)
  EqK us -> 
    let coefVars = catMaybes $ map (\u -> case u of {Const _ -> Nothing;Coef _ i -> Just i}) us in
    let sumConsts = sum $ map (\u -> case u of {Const i -> i;Coef _ _ -> 0}) us in
    case length coefVars of
      0 -> (Nothing,Nothing)
      1 -> case (head coefVars) of 
        1 -> (Just (-sumConsts),Just (-sumConsts))
        -1 -> (Just sumConsts,Just sumConsts)
        _ -> error ("extractRange: unexepcted coefficient: "++show formula)
      _ -> error ("extractRange: unexepcted coefficient: "++show formula)
  _ -> error ("extractRange: unexpected argument: "++show formula)
  where
  intersectRanges:: [Range] -> Range
  intersectRanges l | (length l == 2) = case (l!!0,l!!1) of
    ((Nothing,Just i),(Just j,Nothing)) -> if (i>=j) then (Just j,Just i) else error ("intersectRanges: unexpected argument: "++show l)
    ((Just i,Nothing),(Nothing,Just j)) -> if (j>=i) then (Just i,Just j) else error ("intersectRanges: unexpected argument: "++show l)
    ((Nothing,Nothing),r) -> r
    (r,(Nothing,Nothing)) -> r
    _ -> error ("intersectRanges: unexpected argument: "++show l)
  intersectRanges l | (length l /= 2) = error ("intersectRanges: unexpected argument: "++show l)
    
hausdorffDistance:: (Range,Range) -> Maybe Int
-- ^computes the Hausdorff distance between two intervals. The result Nothing represents Infinity.
hausdorffDistance ((Nothing,Just a), (Nothing,Just b)) = Just (abs (b-a))
hausdorffDistance ((Just a1,Just a2), (Just b1,Just b2)) = Just (abs (b1-a1))
hausdorffDistance ((Just a,Nothing), (Just b,Nothing)) = Just (abs (b-a))
hausdorffDistance (_,_) = Nothing

addHDistances:: [Maybe Int] -> (Int,Int)
-- ^given a list of Hausdorff distances, returns a tuple (m,s), 
-- where m is the number of incompatible dimensions and s is the sum of the distances along the compatible dimensions
addHDistances [] = (0,0)
addHDistances  (Nothing:rest) = let (inc,s) = addHDistances rest in (inc+1,s)
addHDistances ((Just a):rest) = let (inc,s) = addHDistances rest in (inc,s+a)

getDisjuncts:: Formula -> [Formula]
-- requires formula is in DNF-form (result of simplify)
getDisjuncts formula = 
  case formula of
    And _ -> 
      if countDisjuncts formula == 1 then [formula] 
      else error ("getDisjuncts: "++show formula)
    Or fs -> 
      if countDisjuncts formula == length fs then fs 
      else error ("getDisjuncts: "++show formula)
    GEq us -> [formula] 
    EqK us -> [formula]
    AppRecPost mn insouts -> [formula]
    Exists qsvs f -> if countDisjuncts formula == 1 then [formula] else [formula]
    _ -> error ("getDisjuncts: unexpected argument"++show formula)

countDisjuncts:: Formula -> Int
countDisjuncts formula = case formula of
  And fs -> maximum $ map (\f -> countDisjuncts f) fs
  Or fs -> sum (map (\f -> countDisjuncts f) fs)
  GEq us -> 1
  EqK us -> 1
  AppRecPost mn insouts -> 1
  Exists qsvs f -> countDisjuncts f
  _ -> error ("countDisjuncts: unexpected argument: "++show formula)

getConjuncts:: Formula -> [Formula]
-- requires: formula is conjunctive
getConjuncts formula = case formula of
  And fs -> concatMap (\f -> getConjuncts f) fs
  GEq us -> [formula]
  EqK us -> [formula]
  Exists qsvs f -> 
    if countDisjuncts f == 1 then [formula] 
    else error ("getConjuncts: unexpected argument: "++show formula)
  _ -> error ("getConjuncts: unexpected argument: "++show formula)


countConjuncts:: Formula -> Int
-- requires: formula is conjunctive
countConjuncts formula = case formula of
  And fs -> sum (map (\f -> countConjuncts f) fs)
  GEq us -> 1
  EqK us -> 1
  Exists qsvs f -> 
    if countDisjuncts f == 1 then countConjuncts f 
    else error ("countConjuncts: unexpected argument: "++show formula)
  _ -> error ("countConjuncts: unexpected argument: "++show formula)

hullExistentials:: Formula -> FS Formula
hullExistentials disj =
  putStrFS_debug "hullExistentials!" >>
  getFlags >>= \flags -> 
  if (noExistentialsInDisjuncts==True) && (countExis disj > 0) 
  then 
    putStrFS_debug "hullExistentials! ==> hulled" >>    
    putStrFS_DD 1 ("EXISTENTIAL that will be hulled:="++showSet disj) >>
    hull disj
  else return disj

countExis:: Formula -> Int
countExis formula = case formula of
  And fs -> sum (map (\f -> countExis f) fs)
  Or fs -> sum (map (\f -> countExis f) fs)
  GEq us -> 0
  EqK us -> 0
  Exists qsvs f -> 1 + countExis f
  _ -> error ("countExis: unexpected argument: "++show formula)

