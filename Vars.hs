module Vars where

import Type
import Data.List

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    -- Base case, just puts VarName into a list
    allVars (Var a)     = [a]
    -- Applies allVars on every Term concats all the arrays to one and deletes all duplicates.
    allVars (Comb _ ts) =  nub (concatMap allVars ts)

instance Vars Rule where
    -- Applies allVars with same concept as above using the above implementation.
    allVars (Rule n ts) = nub (allVars n ++ concatMap allVars ts)

instance Vars Prog where
    -- Applies allVars for rules like above.
    allVars (Prog rs) = nub (concatMap allVars rs)

instance Vars Goal where
    -- Uses the definition of the instance for Rule above.
    allVars (Goal (t:ts)) = allVars (Rule t ts)
    allVars (Goal [])     = []

-- Creates an endless list of variables matching the pattern of the assignment with lists comprehensions.
freshVars :: [VarName]
freshVars = [(VarName (x : [y])) | y <- ['0'..], x <- ['A' .. 'Z']]
--freshVars = [(VarName (x : "")) | x <- ['A' .. 'Z']] ++ [(VarName (x : [y])) | y <- ['0'..], x <- ['A' .. 'Z']]
