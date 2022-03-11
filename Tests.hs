{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Substitution
import Type
import Vars
import Unifikation
import Umbenennung
import ListFunctions

-- Method to take the Substitution out of a Maybe Substitution
dontBeAMaybeSubst :: Maybe Subst -> Subst
dontBeAMaybeSubst Nothing    = empty
dontBeAMaybeSubst (Just sub) = sub

-- Tests for Substitution

prop_identity_applyOnEmpty :: Term -> Bool
prop_identity_applyOnEmpty t = apply empty t == t

prop_identity_applyOnIdentity :: VarName -> Term -> Bool
prop_identity_applyOnIdentity x t = apply (single x t) (Var x) == t

prop_functionCompProperty :: Term -> Subst -> Subst -> Bool
prop_functionCompProperty t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)

prop_domainOfEmpty :: Bool
prop_domainOfEmpty = domain empty == []

prop_domainOfSelfSubstitution :: VarName -> Bool
prop_domainOfSelfSubstitution x = domain (single x (Var x)) == []

prop_domainOfSingle :: VarName -> Term -> Property
prop_domainOfSingle x t = t /= (Var x) ==> domain (single x t) == [x]

prop_compose_union_subsetContext :: Subst -> Subst -> Bool
prop_compose_union_subsetContext s1 s2 =  (domain (compose s1 s2)) `isSublistOf` ((domain s1) `union` (domain s2))

prop_domainOfCircles :: VarName -> VarName -> Property
prop_domainOfCircles x1 x2 = x1 /= x2 ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2]

prop_varsOfEmpty :: Bool
prop_varsOfEmpty = allVars empty == []

prop_varsOfSelfSubstitution :: VarName -> Bool
prop_varsOfSelfSubstitution x = allVars (single x (Var x)) == []

prop_varsOfSubstitution :: VarName -> Term -> Property
prop_varsOfSubstitution x t = t /= (Var x) ==> allVars (single x t) `isEq` (allVars t `union` [x])

prop_varsOfCompose :: Subst -> Subst -> Bool
prop_varsOfCompose s1 s2 = (allVars (compose s1 s2)) `isSublistOf` (allVars s1 `union` allVars s2)

prop_varsOfComposedCircle :: VarName -> VarName -> Property
prop_varsOfComposedCircle x1 x2 = x1 /= x2 ==> allVars (compose (single x2 (Var x1)) (single x1 (Var x2))) `isEq` [x1, x2]

prop_domainIsSubsetOfVars :: Subst -> Bool
prop_domainIsSubsetOfVars s = (domain s) `isSublistOf` (allVars s)

prop_domainOfRestrictOfEmpty :: [VarName] -> Bool
prop_domainOfRestrictOfEmpty xs = domain (restrictTo empty xs) == []

prop_domain_subset_of_restrictionDomain :: [VarName] -> Subst -> Bool
prop_domain_subset_of_restrictionDomain xs s = domain (restrictTo s xs) `isSublistOf` xs

-- Tests for Unification --

prop_dsOfSameTerm :: Term -> Bool
prop_dsOfSameTerm t = ds t t == Nothing

prop_dsOfDiffTerm :: Term -> Term -> Property
prop_dsOfDiffTerm t1 t2 = ds t1 t2 /= Nothing ==> t1 /= t2

prop_unifyWithEmptyDs :: Term -> Term -> Property
prop_unifyWithEmptyDs t1 t2 = ds t1 t2 == Nothing ==> unify t1 t2 /= Nothing && domain (dontBeAMaybeSubst (unify t1 t2)) == []

prop_successfullUnify :: Term -> Term -> Property
prop_successfullUnify t1 t2 = (unify t1 t2) /= Nothing ==> (ds (apply (dontBeAMaybeSubst (unify t1 t2)) t1) (apply (dontBeAMaybeSubst (unify t1 t2)) t2) == Nothing)


-- Tests für Umbenennung

prop_allNewVars :: [VarName] -> Rule -> Bool
prop_allNewVars xs r = (intersect (allVars (rename xs r)) (allVars r)) == []

prop_noForbiddenVars :: [VarName] -> Rule -> Bool
prop_noForbiddenVars xs r = (intersect (allVars (rename xs r)) xs) == []

prop_noAnonymusVars :: [VarName] -> Rule -> Bool
prop_noAnonymusVars xs r = not (elem (VarName "_") (allVars (rename xs r)))

prop_sameVarNumberIfNoAnonymusVars :: [VarName] -> Rule -> Property
prop_sameVarNumberIfNoAnonymusVars xs r = not (elem (VarName "_") (allVars r)) ==> (length (allVars (rename xs r))) == (length (allVars r))

prop_varNumberWithAnonymusVars :: [VarName] -> Rule -> Bool
prop_varNumberWithAnonymusVars xs r = (length (allVars (rename xs r))) >= (length (allVars r))


return []
runTests :: IO Bool
runTests = $quickCheckAll
