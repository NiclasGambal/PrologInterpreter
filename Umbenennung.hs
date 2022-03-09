module Umbenennung(rename) where

import Type

import Vars

import Substitution

-- GrundIdee: Nehme alle Variablen, erstelle "Umbenennungssubstitutionen" und wende sie auf alle Terme an
rename :: [VarName] -> Rule -> Rule
rename varnames (Rule t ts) = renameAnonymRule (drop (length (allVars (Rule t ts))) (filter (\n -> (not (elem n (varnames ++ (allVars (Rule t ts)))))) freshVars)) (Rule (apply subsToDo t) (map (\x -> (apply subsToDo x)) ts))
  where
    subsToDo = (buildSub (filter (/= VarName "_") (allVars (Rule t ts))) (filter (\n -> (not (elem n (varnames ++ (allVars (Rule t ts)))))) freshVars))
    buildSub :: [VarName] -> [VarName] -> Subst
    buildSub (x:xs) (y:ys) = compose (single x (Var y)) (buildSub xs ys)
    buildSub _      _      = empty
    renameAnonymTerm :: [VarName] -> Term -> Term
    renameAnonymTerm [] _                   = Var (VarName "hier solltest du nicht sein")
    renameAnonymTerm li (Var (VarName "_")) = Var (head li)
    renameAnonymTerm _  (Var (VarName  l )) = Var (VarName l)
    renameAnonymTerm li (Comb a tx)         = Comb a (renameAnonymList li tx)
    renameAnonymList :: [VarName] -> [Term] -> [Term]
    renameAnonymList _  []                  = []
    renameAnonymList li (el:els)            = (renameAnonymTerm li el):(renameAnonymList (drop (numberOf_ el) li) els)
    renameAnonymRule :: [VarName] -> Rule -> Rule
    renameAnonymRule li (Rule a as) = Rule (renameAnonymTerm li a) (renameAnonymList (drop (numberOf_ a) li) as)

numberOf_ :: Term -> Int
numberOf_ (Var (VarName "_")) = 1
numberOf_ (Var (VarName  _ )) = 0
numberOf_ (Comb _ tx)     = sum (map numberOf_ tx)
{-
renameAnonym :: [Term] -> Int -> [Term]
renameAnonym []                       _ = []
renameAnonym ((Var (VarName "_")):re) i = ((Var (VarName (show i ++ "_"))):(renameAnonym re (i + 1)))
renameAnonym ((Var (VarName  l )):re) i = (Var (VarName ("_" ++ l))):(renameAnonym re i)
renameAnonym ((Comb u rs):re)         i = (Comb u (renameAnonym rs i)):(renameAnonym re (i + (sum (map numberOf_ re))))
-}
