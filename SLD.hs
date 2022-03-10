module SLD where

import Type

import Substitution

import Unifikation

data SLDTree = Node Goal [(Subst, SLDTree)]
    deriving Show

type Strategy = SLDTree -> [Subst]

sld :: Prog -> Goal -> SLDTree
sld _ (Goal []) = Node (Goal []) []
sld (Prog (r:rs)) g = Node g (nodes p g) (concatMap nodes p )

nodes :: Prog -> Goal -> [(Subst, SLDTree)]
nodes (Prog (r:rs)) g = [(createSubst r g, sld)] ++ nodes (Prog rs) g

rightSide :: Rule -> Rule 
rightSide (Rule r ts) =

createSubst :: Rule -> Goal -> Subst
createSubst (Rule t1 _) (Goal (t:ts)) = dontBeAMaybeSubst (unify t1 t)
--sameName :: Idee vergleichen ob Rule t gleich t aus Goal

applySubst :: Subst -> Term -> Rule
applySubst s r (Rule )= if apply s r == 


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