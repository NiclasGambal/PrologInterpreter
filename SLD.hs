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

prog1 :: Prog
prog1 = Prog [(Rule (Comb "p" [(Var (VarName "X")),(Var (VarName "Z"))]) [(Comb "q" [(Var (VarName "X")),(Var (VarName "Y"))]),(Comb "p" [(Var (VarName "Y")),(Var (VarName "Z"))])]),(Rule (Comb "p" [(Var (VarName "X")),(Var (VarName "X"))]) []),(Rule (Comb "q" [(Comb "a" []),(Comb "b" [])]) [])]

goal1 :: Goal
goal1 = Goal [(Comb "p" [(Var (VarName "S")),(Comb "b" [])])]

term1 :: Term
term1 =  Comb "p" [((Comb "b" [])),(Comb "b" [])]

term2 :: Term 
term2 = Comb "p" [(Var (VarName "S")),(Comb "b" [])]

rule1 :: Rule
rule1 = (Rule (Comb "p" [(Var (VarName "X")),(Var (VarName "Z"))]) [(Comb "q" [(Var (VarName "X")),(Var (VarName "Y"))]),(Comb "p" [(Var (VarName "Y")),(Var (VarName "Z"))])])

rule2 :: Term 
rule2 = Comb "p" [(Var (VarName "X")),(Var (VarName "X"))] 