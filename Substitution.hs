module Substitution (Subst, domain, empty, single, apply, compose, restrictTo) where

import Type

import PrettyPrinting

import Vars
import Data.List

data Subst = Subst[(VarName, Term)]
    deriving Show

-- Returns the domain of the substitution
domain :: Subst -> [VarName]
-- Gets all first elements of the substitution. 
domain (Subst ps) = nub (concatMap comparer ps)

-- Checks if the vars of domain picture themself.
comparer :: (VarName, Term) -> [VarName]
comparer (v, Var n) = if v == n then [] else [v]

-- Creates an empty substitution
empty :: Subst
empty = Subst []

-- Creates an substitution which maps only one var on a term
single :: VarName -> Term -> Subst
-- Checks if the vars maps with itself
single v (Var n) | v == n = empty
single v t = Subst[(v,t)]

-- Applies an substitution on a term
apply :: Subst -> Term -> Term
apply (Subst[]) t = t
-- Applies a substitution, checking if the substitution var appears in the term, otherwise move to the next.
apply (Subst ((v,t):rs)) (Var n) = if v == n then t else apply (Subst rs) (Var n)
-- Keeps the frame of the Term just apply the substitution on equal named VarNames.
apply s (Comb n ts) = (Comb n (map (apply s) ts))

-- Composes two substitutions
compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) = Subst (appliedSet ++ filteredSet) where
    -- Applies substitution 1 to every term substitution in 2.
    appliedSet = (map (\(v, t) -> (v, apply (Subst s1) t)) s2)
    -- filter the elements where the Var from substitution 1 is the same as in substitution 2.
    filteredSet = filter (\(v,_) -> (not (elem v (map fst s2)))) s1

-- Restricts a substitution to a defined domain
restrictTo :: Subst -> [VarName] -> Subst
-- Base case, restriction doesnt matter.
restrictTo (Subst []) _ = empty
-- if a var is in the domain, keep this var and move on, otherwise ignore that var and move on.
restrictTo (Subst (p:ps)) dm | (elem (fst p) dm) = let Subst xs = restrictTo (Subst ps) dm in (Subst (p:xs))
                             | otherwise = restrictTo (Subst ps) dm

-- Instance of Subst of classtype Pretty
instance Pretty Subst where
    pretty (Subst []) = "{}"
    pretty (Subst ((v,t):ps)) = "{" ++ (pretty (Var v)) ++ " -> " ++ (pretty t) ++"}"
        where rest = undefined 