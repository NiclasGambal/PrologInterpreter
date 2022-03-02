module Substitution (Subst, domain, empty, single, compose) where

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
comparer (v, t) = if v `elem` allVars t then [] else [v]

-- Creates an empty substitution
empty :: Subst
empty = Subst []

-- Creates an substitution which maps only one var on a term
single :: VarName -> Term -> Subst
single v t = Subst[(v,t)]

-- Applies an substitution on a term
--apply :: Subst -> Term -> Term
--apply s t = undefined 

-- Composes two substitutions
compose :: Subst -> Subst -> Subst
compose s1 s2 = undefined 

-- Restricts a substitution to a defined domain
--restrictTo :: Subst -> [VarName] -> Subst
--restrictTo x ys = undefined

-- Instance of Subst of classtype Pretty
instance Pretty Subst where
    pretty (Subst []) = "{}"