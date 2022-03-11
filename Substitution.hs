module Substitution (Subst(Subst), domain, empty, single, apply, compose, restrictTo) where

import Type
import PrettyPrinting
import Vars
import ListFunctions
import Test.QuickCheck

data Subst = Subst [(VarName, Term)]
    deriving (Show, Eq)

-- Returns the domain of the substitution
domain :: Subst -> [VarName]
-- Gets all first elements of the substitution.
domain (Subst ps) = concatMap comparer ps

-- Checks if the vars of domain picture themself.
comparer :: (VarName, Term) -> [VarName]
comparer (v, Var n) = if v == n then [] else [v]
comparer (v, _) = [v]

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
apply (Subst []) t = t
-- Applies a substitution, checking if the substitution var appears in the term, otherwise move to the next.
apply (Subst ((v,t):rs)) (Var n) = if v == n then t else apply (Subst rs) (Var n)
-- Keeps the frame of the Term just apply the substitution on equal named VarNames.
apply s (Comb n ts) = Comb n (map (apply s) ts)

-- Composes two substitutions
compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) = Subst (appliedSet ++ filteredSet) where
    -- Applies substitution 1 to every term  of the substitutions in 2 and filters ids.
    appliedSet = filter (\(v,t) -> Var v /= t) (map (\(v, t) -> (v, apply (Subst s1) t)) s2)
    -- filter the elements where the Var from substitution 1 is the same as in substitution 2.
    filteredSet = filter (\(v,_) -> (not (elem v (domain (Subst s2))))) s1

-- Restricts a substitution to a defined domain
restrictTo :: Subst -> [VarName] -> Subst
-- Base case, restriction doesnt matter.
restrictTo (Subst []) _ = empty
-- if a var is in the domain, keep this var and move on, otherwise ignore that var and move on.
restrictTo (Subst s) dm = Subst (filter (\(v,_) -> elem v dm) s)

instance Pretty Subst where
    -- Use intercalate for the inserted ", " and maps pretty on the substitutions.
    pretty (Subst ps) = "{" ++ intercalate ", " (map (\(v,t) -> pretty (Var v) ++ " -> " ++ pretty t) ps) ++ "}"

instance Vars Subst where
    allVars (Subst[]) = []
    -- Just applies allVars on the single parts of the tuples and eliminates all duplicates.
    allVars (Subst ((v,t):rs)) = nub (allVars (Var v) ++ (allVars t) ++ (allVars (Subst rs)))

instance Arbitrary Subst where
    arbitrary = do x <- arbitrary
                   y <- arbitrary
                   return (Subst [(x, y)])
