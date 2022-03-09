module SLD where

import Type

import Substitution

data SLDTree = Node Goal [(Subst, SLDTree)]
    deriving Show

type Strategy = SLDTree -> [Subst]

sld :: Prog -> Goal -> SLDTree
sld p g = undefined 

dfs :: Strategy
dfs = undefined

bfs :: Strategy
bfs = undefined 

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g strat = undefined