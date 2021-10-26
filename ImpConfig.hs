{- | 
  Lists all the configuration flags.
-}

module ImpConfig(
  Flags(..),
  Hull(..), 
  Prederivation(..),
  Postcondition(..),
  Heur(..),
  FixFlags,
  defaultFlags,
  noExistentialsInDisjuncts,
  simulateOldFixpoint
) where

-- import Text.Regex.Base

data Hull = Hull | ConvexHull deriving Show
data Prederivation = WeakPD | StrongPD | SelectivePD | PostPD | DualPD deriving (Show,Eq)
data Postcondition = StrongPost | WeakPost deriving (Show,Eq)
type FixFlags = (Int,Heur)
data Heur = SimilarityHeur | DifferenceHeur | HausdorffHeur | SimInteractiveHeur deriving (Show,Eq)

data Flags = Flags {
  noInference:: Bool,
  checkingAfterInference:: Bool,
  isIndirectionIntArray:: Bool, -- ^Collect constraints regarding Min and Max values of the array elements. Default is False.
  outputFile:: String,
  showDebugMSG :: Int ,
{- | 0 -> do not show any fixpoint messages
     1 -> show only loss-of-precision messages
     2 -> show loss-of-precision and hull-widening messages -}
  prederivation:: Prederivation, -- ^Kind of prederivation. Default is PostPD.
  postcondition:: Postcondition, -- ^Whether to accumulate preconditions in the computed postcondition. Default is True.
  traceIndividualErrors:: Bool,  -- ^Trace individual errors for Dual Analysis.
  computeAll:: Bool,             -- ^Compute all safety/bug conditions for all methods (flag:True) or only for external methods (flag:False).
  fixFlags:: FixFlags,           -- ^Number of disjuncts (m) and heuristic function to compute disjunct affinity. Default is (5, Similarity).
  whatHull:: Hull,               -- ^What least upper bound operator: Hull or ConvexHull. Default is Hull.
  simplifyCAbst:: Bool,          -- ^Whether to simplify the constraint abstraction before fixpoint (experimental flag).
---- derive 2 stronger preconditions that need specialization for recursive functions
---- otherwise the resulting program may not type-check
  separateFstFromRec:: Bool,
  showDRE :: Maybe String,                  -- for method-level debugging
  useSelectiveHull:: Bool,       -- ^Used by the old fixpoint. Quicksort (Hanoi and Mergesort) require selectiveHull for precise result.
  widenEarly:: Bool              -- ^Used by the old fixpoint. Quicksort requires widenEarly for precise result.
} deriving Show

defaultFlags = Flags { 
  noInference = False,
  checkingAfterInference = False,
  isIndirectionIntArray = False,
  outputFile = "a",
  showDebugMSG = 0,
  prederivation = DualPD,
  postcondition = StrongPost, 
  traceIndividualErrors = False,
  computeAll = True,
  fixFlags = (5,SimilarityHeur),
  whatHull = Hull,
  simplifyCAbst = False,
  separateFstFromRec = False,
  showDRE = Nothing,
  useSelectiveHull = False,
  widenEarly = True
}

noExistentialsInDisjuncts = True
simulateOldFixpoint = False 
-- ^The disjunctive fixpoint procedure implemented before ASIAN'06 (no affinity, only separate base from recursive).
