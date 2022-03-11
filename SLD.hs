module SLD where

import Type
import Vars
import Umbenennung
import Substitution
import Unifikation

data SLDTree = Node Goal [(Subst, SLDTree)]
    deriving Show

-- creates an SLDTree to a given prog and goal using "First"-strategy
sld :: Prog -> Goal -> SLDTree
sld p g = sldRename ((allVars p) ++ (allVars g)) p g

-- To Rename all variables and not use names that are already taken, this method is the "sld"-method, but carries all taken names with it.
-- In our tests, it could also work with only renaming the new Rule, but this is a little bit safer.
sldRename :: [VarName] -> Prog -> Goal -> SLDTree
sldRename _  _            (Goal []) = Node (Goal []) []
-- try appling the Ruleset to the first literal
sldRename vn (Prog rules) (Goal (t:ts)) = Node (Goal (t:ts)) (concatMap (\oneRule -> (tryRule vn (Prog rules) (Goal (t:ts)) (rename vn oneRule) t)) rules)
-- try to apply one Rule to a Term
tryRule :: [VarName] -> Prog -> Goal -> Rule -> Term -> [(Subst, SLDTree)]
tryRule vn p (Goal ts) (Rule rHead rTail) t = case unify rHead t of
                                                Nothing -> []
                                                Just s ->  [(s, sldRename (vn ++ (allVars (Rule rHead rTail))) p (Goal (map (apply s) (filter (/= t) ts ++ rTail))))]


-- Method to take the Substitution out of a Maybe Substitution
dontBeAMaybeSubst :: Maybe Subst -> Subst
dontBeAMaybeSubst Nothing    = empty
dontBeAMaybeSubst (Just sub) = sub

type Strategy = SLDTree -> [Subst]

-- searches for all solutions of a SLDTree using dfs
dfs :: Strategy
dfs (Node (Goal []) _) = [empty]
dfs (Node _ []) = []
dfs (Node g ((s,t):rs)) = (map (\x -> restrictTo (compose x s) (allVars g)) (dfs t) ++ dfs (Node g rs))

-- searches for all solutions of a SLDTree using bfs
bfs :: Strategy
bfs (Node _ []) = []
bfs (Node g kn) = map (\(s,_) -> (restrictTo s (allVars g))) (filter (\(_,Node (Goal ts) _) -> ts == []) kn) ++ bfs (Node g (concatMap (\(su,(Node _ ksr)) -> map (\(a,b) -> ((compose a su),b)) ksr) kn))

-- solves a calculation using an SLDTree and one of the strategies
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g strat = strat (sld p g)

prog1 :: Prog
prog1 = Prog [(Rule (Comb "p" [(Var (VarName "X")),(Var (VarName "Z"))]) [(Comb "q" [(Var (VarName "X")),(Var (VarName "Y"))]),(Comb "p" [(Var (VarName "Y")),(Var (VarName "Z"))])]),(Rule (Comb "p" [(Var (VarName "X")),(Var (VarName "X"))]) []),(Rule (Comb "q" [(Comb "a" []),(Comb "b" [])]) [])]

goal1 :: Goal
goal1 = Goal [(Comb "p" [(Var (VarName "S")),(Comb "b" [])])]
