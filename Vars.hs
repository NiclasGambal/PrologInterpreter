module Vars where

import Type 
import Data.List

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    -- Base case
    allVars (Var a) = [a]
    allVars (Comb _ ts) =  nub (concatMap allVars ts)

instance Vars Rule where
    allVars (Rule n [])     = allVars n
    allVars (Rule n ts) = nub ((allVars n) ++ (concatMap allVars ts))

instance Vars Prog where
    allVars (Prog [])       = []
    allVars (Prog rs)   = nub (concatMap allVars rs)

instance Vars Goal where
    allVars (Goal []) = []
    allVars (Goal (t:ts)) = (allVars (Rule t ts))