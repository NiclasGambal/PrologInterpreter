module Unifikation where

import Type
import Vars

import Substitution

-- calculates the disagreement-set of two terms
ds :: Term -> Term -> Maybe (Term, Term)
-- case one is a variable, you have your ds. Check for anonymus variables (then it's empty)
ds (Var vn1)      t2             = if ((t2 == (Var vn1)) || (t2 == (Var (VarName "_"))) || (vn1 == (VarName "_"))) then Nothing else (Just ((Var vn1), t2))
ds t2             (Var vn1)      = if ((t2 == (Var vn1)) || (t2 == (Var (VarName "_"))) || (vn1 == (VarName "_"))) then Nothing else (Just ((Var vn1), t2))
-- case both a comb-terms, if they have the same combname and list-lenght, check vor the first ds in the list, if not, the terms are the ds
ds (Comb cn1 ts1) (Comb cn2 ts2) = if ((cn1 == cn2) && ((length ts1 == length ts2))) then (getFirstDs ts1 ts2) else (Just ((Comb cn1 ts1), (Comb cn2 ts2)))
  where
    getFirstDs []       []       = Nothing
    getFirstDs (x1:xs1) (x2:xs2) = if ((ds x1 x2) == Nothing) then (getFirstDs xs1 xs2) else (ds x1 x2)
    getFirstDs _        _        = Nothing

-- calculates the Substitution to unify two terms, Nothing if the terms can't be unified
unify :: Term -> Term -> Maybe Subst
-- first, it calculates the ds of the two terms
unify t1 t2 = unifyAkk t1 t2 (ds t1 t2)
  where
    -- case the Terms are already unified
    unifyAkk _  _  Nothing               = (Just empty)
    -- case apply the substitution, then move forward. Here, an occours-check is needed to prevent infinit loops
    unifyAkk x1 x2 (Just ((Var vn), tx)) = if (elem vn (allVars tx)) then Nothing else combineUnify (single vn tx) (unify (apply (single vn tx) x1) (apply (single vn tx) x2))
    unifyAkk x1 x2 (Just (tx, (Var vn))) = if (elem vn (allVars tx)) then Nothing else combineUnify (single vn tx) (unify (apply (single vn tx) x1) (apply (single vn tx) x2))
    -- case the Terms can't be unified
    unifyAkk _  _  _                     = Nothing
    -- just a Method to compose with Maybe Subst
    combineUnify :: Subst -> Maybe Subst -> Maybe Subst
    combineUnify _ Nothing = Nothing
    combineUnify s1 (Just s2) = Just (compose s2 s1)
