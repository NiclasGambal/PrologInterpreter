module Unifikation where

import Type

import PrettyPrinting

import Vars

import Substitution

import Data.List
import Test.QuickCheck


ds :: Term -> Term -> Maybe (Term, Term)
ds (Var vn1)      t2             = if (t2 == (Var vn1)) then Nothing else (Just ((Var vn1), t2))
ds (Comb cn1 ts1) (Comb cn2 ts2) = if (cn1 == cn2) then (getFirstDs ts1 ts2) else (Just ((Comb cn1 ts1), (Comb cn2 ts2)))
  where
    getFirstDs []       []       = Nothing
    getFirstDs (x1:xs1) (x2:xs2) = if ((ds x1 x2) == Nothing) then (getFirstDs xs1 xs2) else (ds x1 x2)
