module Unifikation where

import Type
import Vars

import Substitution


ds :: Term -> Term -> Maybe (Term, Term)
ds (Var vn1)      t2             = if ((t2 == (Var vn1)) || (t2 == (Var (VarName "_"))) || (vn1 == (VarName "_"))) then Nothing else (Just ((Var vn1), t2))
ds t2             (Var vn1)      = if ((t2 == (Var vn1)) || (t2 == (Var (VarName "_"))) || (vn1 == (VarName "_"))) then Nothing else (Just ((Var vn1), t2))
ds (Comb cn1 ts1) (Comb cn2 ts2) = if ((cn1 == cn2) && ((length ts1 == length ts2))) then (getFirstDs ts1 ts2) else (Just ((Comb cn1 ts1), (Comb cn2 ts2)))
  where
    getFirstDs []       []       = Nothing
    getFirstDs (x1:xs1) (x2:xs2) = if ((ds x1 x2) == Nothing) then (getFirstDs xs1 xs2) else (ds x1 x2)
    getFirstDs _        _        = Nothing


unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unifyAkk t1 t2 (ds t1 t2)
  where
    unifyAkk _  _  Nothing               = (Just empty)
    unifyAkk x1 x2 (Just ((Var vn), tx)) = if (elem vn (allVars tx)) then Nothing else combineUnify (single vn tx) (unify (apply (single vn tx) x1) (apply (single vn tx) x2))
    unifyAkk x1 x2 (Just (tx, (Var vn))) = if (elem vn (allVars tx)) then Nothing else combineUnify (single vn tx) (unify (apply (single vn tx) x1) (apply (single vn tx) x2))
    unifyAkk _  _  _                     = Nothing
    combineUnify :: Subst -> Maybe Subst -> Maybe Subst
    combineUnify _ Nothing = Nothing
    combineUnify s1 (Just s2) = Just (compose s1 s2)
