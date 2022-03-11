module Umbenennung(rename) where

import Type
import Vars
import Substitution

-- GrundIdee: Nehme alle Variablen, erstelle "Umbenennungssubstitutionen" und wende sie auf alle Terme an

-- renames all variables in a Rule using the names from "freshVars". Does not use names, that a also part of the Rule or the list of names
-- all anonymus variables get a unic new name
rename :: [VarName] -> Rule -> Rule
rename varnames (Rule t ts) = renameAnonymRule (drop (length (allVars (Rule t ts))) (filter (\n -> (not (elem n (varnames ++ (allVars (Rule t ts)))))) freshVars)) (Rule (apply subsToDo t) (map (\x -> (apply subsToDo x)) ts))
  where
    subsToDo = (buildSub (filter (/= VarName "_") (allVars (Rule t ts))) (filter (\n -> (not (elem n (varnames ++ (allVars (Rule t ts)))))) freshVars))
    buildSub :: [VarName] -> [VarName] -> Subst
    buildSub (x:xs) (y:ys) = compose (single x (Var y)) (buildSub xs ys)
    buildSub _      _      = empty
    -- this stuff is needet, to give all anonymus variables new names (jup, it's ugly)
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

-- calculates the number of anonym variables in a term
numberOf_ :: Term -> Int
numberOf_ (Var (VarName "_")) = 1
numberOf_ (Var (VarName  _ )) = 0
numberOf_ (Comb _ tx)     = sum (map numberOf_ tx)
