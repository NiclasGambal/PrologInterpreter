module Substitution where

import Type

import PrettyPrinting

data Subst = Subst[(Term, VarName)]
    deriving Show

-- Returns the domain of the substitution
domain :: Subst -> [VarName]
domain (Subst []) = []
domain (Subst (p:ps)) = [snd p] ++ (domain (Subst ps))

-- Creates an empty substitution
empty :: Subst
empty = Subst []

-- Creates an substitution which maps only one var on a term
--single :: VarName -> Term -> Subst
--single x y = undefined

-- Applies an substitution on a term
--apply :: Subst -> Term -> Term
--apply s t = undefined 

-- Composes two substitutions
--compose :: Subst -> Subst -> Subst
--compose x y = undefined

-- Restricts a substitution to a defined domain
--restrictTo :: Subst -> [VarName] -> Subst
--restrictTo x ys = undefined

-- Instance of Subst of classtype Pretty
instance Pretty Subst where
    pretty (Subst []) = "{}"