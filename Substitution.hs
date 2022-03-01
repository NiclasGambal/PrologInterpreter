module Substitution where

import Type

import PrettyPrinting

data Subst = Subst[(VarName, Term)]
    deriving Show

-- Returns the domain of the substitution
domain :: Subst -> [VarName]
domain x = undefined

-- Creates an empty substitution
empty :: Subst
empty = undefined 

-- Creates an substitution which maps only one var on a term
single :: VarName -> Term -> Subst
single x y = undefined

-- Applies an substitution on a term
apply :: Subst -> Term -> Term
apply s t = undefined 

-- Composes two substitutions
compose :: Subst -> Subst -> Subst
compose x y = undefined

-- Restricts a substitution to a defined domain
restrictTo :: Subst -> [VarName] -> Subst
restrictTo x ys = undefined

instance Pretty Subst where
    pretty (Subst []) = "{}"