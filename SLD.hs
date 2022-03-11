module SLD where

import Type

import Vars
import Umbenennung
import Substitution

import Unifikation
import PrettyPrinting

data SLDTree = Node Goal [(Subst, SLDTree)]
    deriving Show


sld :: Prog -> Goal -> SLDTree
sld _            (Goal []) = Node (Goal []) []
-- versuche, das Regelset auf jeden Term anzuwenden
sld (Prog rules) (Goal (t:ts)) = Node (Goal (t:ts)) (concatMap (\oneRule -> (tryRule (Prog rules) (Goal (t:ts)) (rename ((allVars (Prog rules)) ++ (allVars (Goal (t:ts)))) oneRule) t)) rules)


tryRule:: Prog -> Goal -> Rule -> Term -> [(Subst, SLDTree)]
tryRule p (Goal ts) (Rule rHead rTail) t = case unify rHead t of
                                                Nothing -> []
                                                Just s ->  [(s, sld p (Goal (map (apply s) (filter (/= t) ts ++ rTail))))]



-- Method to take the Substitution out of a Maybe Substitution
dontBeAMaybeSubst :: Maybe Subst -> Subst
dontBeAMaybeSubst Nothing    = empty
dontBeAMaybeSubst (Just sub) = sub

type Strategy = SLDTree -> [Subst]

dfs :: Strategy
dfs (Node (Goal []) _) = [empty]
dfs (Node _ []) = []
dfs (Node g ((s,t):rs)) = (map (\x -> restrictTo (compose x s) (allVars g)) (dfs t) ++ dfs (Node g rs))

bfs :: Strategy
bfs (Node _ []) = []
bfs (Node g kn) = map (\(s,_) -> (restrictTo s (allVars g))) (filter (\(_,Node (Goal ts) _) -> ts == []) kn) ++ bfs (Node g (concatMap (\(su,(Node _ ksr)) -> map (\(a,b) -> ((compose a su),b)) ksr) kn))

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g strat = strat (sld p g)

prog1 :: Prog
prog1 = Prog [(Rule (Comb "p" [(Var (VarName "X")),(Var (VarName "Z"))]) [(Comb "q" [(Var (VarName "X")),(Var (VarName "Y"))]),(Comb "p" [(Var (VarName "Y")),(Var (VarName "Z"))])]),(Rule (Comb "p" [(Var (VarName "X")),(Var (VarName "X"))]) []),(Rule (Comb "q" [(Comb "a" []),(Comb "b" [])]) [])]

goal1 :: Goal
goal1 = Goal [(Comb "p" [(Var (VarName "S")),(Comb "b" [])])]