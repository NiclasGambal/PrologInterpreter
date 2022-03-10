module SLD where

import Type

import Substitution

import Unifikation

data SLDTree = Node Goal [(Subst, SLDTree)]
    deriving Show

type Strategy = SLDTree -> [Subst]
{-
sld :: Prog -> Goal -> SLDTree
sld (Prog (r:rs)) (Goal (t:ts)) = Node (Goal (t:ts)) [let s = unifyGoals r  (Goal (t:ts)) in s, sld (Prog r:rs) applyOnAll s (Goal (t:ts))] ++ (next (Prog rs) (Goal t:ts))

next :: Prog -> Goal -> [(Subst, SLDTree)]
next (Prog []) _ = []
next (Prog r:rs ) g = (let s = unifyGoals r  (Goal (t:ts)) in s,sld (Prog r:rs applyOnAll s g) ++ next (Prog rs) g

applyOnAll :: Subst -> Goal -> Goal
applyOnAll s (Goal ts) = Goal (map (apply s ) ts)
-}

unifyGoals :: Rule -> Goal -> Subst
unifyGoals _ (Goal []) = empty
unifyGoals (Rule r rs) (Goal (t:ts)) =  compose (dontBeAMaybeSubst (unify r t)) (unifyGoals (Rule r rs) (Goal ts))

-- Method to take the Substitution out of a Maybe Substitution
dontBeAMaybeSubst :: Maybe Subst -> Subst
dontBeAMaybeSubst Nothing    = empty
dontBeAMaybeSubst (Just sub) = sub

{-
dfs :: Strategy
dfs (Node (Goal []) _) = [empty]
dfs (Node _ []) = []

bfs :: Strategy
bfs = undefined 

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g strat = strat (sld p g)
-}