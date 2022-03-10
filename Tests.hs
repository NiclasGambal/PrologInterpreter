{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import PrettyPrinting
import Substitution
import Type
import Vars
import Unifikation
import Umbenennung
import Data.List
import Debug.Trace

-- Method to check if a list is a subset of another list.
isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf a b = all (\n -> elem n b) a

isEq :: Eq a => [a] -> [a] -> Bool
isEq a b = isSublistOf a b && isSublistOf b a
{-
isSublistOf [] [] = True
isSublistOf _  [] = False
isSublistOf []  _ = True
isSublistOf (x:xs) (y:ys) | x == y    = isSublistOf xs ys
                          | otherwise = isSublistOf (x:xs) ys
-}

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

prop_5 :: VarName -> Bool
prop_5 x = domain (single x (Var x)) == []

prop_6 :: VarName -> Term -> Property
prop_6 x t = t /= (Var x) ==> domain (single x t) == [x]

prop_compose_union_subsetContext :: Subst -> Subst -> Bool
prop_compose_union_subsetContext s1 s2 =  (domain (compose s1 s2)) `isSublistOf` ((domain s1) `union` (domain s2))

prop_8 :: VarName -> VarName -> Property
prop_8 x1 x2 = x1 /= x2 ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2]

prop_9 :: Bool
prop_9 = allVars empty == []

prop_10 :: VarName -> Bool
prop_10 x = allVars (single x (Var x)) == []

prop_11 :: VarName -> Term -> Property
prop_11 x t = t /= (Var x) ==> allVars (single x t) `isEq` (allVars t `union` [x])

prop_12 :: Subst -> Subst -> Bool
prop_12 s1 s2 = (allVars (compose s1 s2)) `isSublistOf` (allVars s1 `union` allVars s2)

prop_13 :: VarName -> VarName -> Property
prop_13 x1 x2 = x1 /= x2 ==> allVars (compose (single x2 (Var x1)) (single x1 (Var x2))) `isEq` [x1, x2]

prop_14 :: Subst -> Bool
prop_14 s = (domain s) `isSublistOf` (allVars s)

prop_15 :: [VarName] -> Bool
prop_15 xs = domain (restrictTo empty xs) == []

prop_domain_subset_of_restrictionDomain :: [VarName] -> Subst -> Bool
prop_domain_subset_of_restrictionDomain xs s = domain (restrictTo s xs) `isSublistOf` xs

-- Tests for Unification --

prop_u1 :: Term -> Bool
prop_u1 t = ds t t == Nothing

prop_u2 :: Term -> Term -> Property
prop_u2 t1 t2 = ds t1 t2 /= Nothing ==> t1 /= t2

prop_u3 :: Term -> Term -> Property
prop_u3 t1 t2 = ds t1 t2 == Nothing ==> unify t1 t2 /= Nothing && domain (dontBeAMaybeSubst (unify t1 t2)) == []

prop_u4 :: Term -> Term -> Property
prop_u4 t1 t2 = (unify t1 t2) /= Nothing ==> (ds (apply (dontBeAMaybeSubst (unify t1 t2)) t1) (apply (dontBeAMaybeSubst (unify t1 t2)) t2) == Nothing)


-- Tests für Umbenennung

prop_um1 :: [VarName] -> Rule -> Bool
prop_um1 xs r = (intersect (allVars (rename xs r)) (allVars r)) == []

prop_um2 :: [VarName] -> Rule -> Bool
prop_um2 xs r = (intersect (allVars (rename xs r)) xs) == []

prop_um3 :: [VarName] -> Rule -> Bool
prop_um3 xs r = not (elem (VarName "_") (allVars (rename xs r)))

prop_um4 :: [VarName] -> Rule -> Property
prop_um4 xs r = not (elem (VarName "_") (allVars r)) ==> (length (allVars (rename xs r))) == (length (allVars r))

prop_um5 :: [VarName] -> Rule -> Bool
prop_um5 xs r = (length (allVars (rename xs r))) >= (length (allVars r))


return []
runTests :: IO Bool
runTests = $quickCheckAll
