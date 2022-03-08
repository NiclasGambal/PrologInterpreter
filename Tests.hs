{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Substitution
import Type
import Vars


prop_identity_applyOnEmpty :: Term -> Bool 
prop_identity_applyOnEmpty t = apply empty t == t

prop_identity_applyOnIdentity :: VarName -> Term -> Bool 
prop_identity_applyOnIdentity x t = apply (single x t) (Var x) == t

prop_3 :: Term -> Subst -> Subst -> Bool 
prop_3 t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)

prop_4 :: Bool
prop_4 = domain empty == []

prop_5 :: VarName -> Bool 
prop_5 x = domain (single x (Var x)) == []

prop_6 :: VarName -> Term -> Property 
prop_6 x t = t /= (Var x) ==> domain (single x t) == [x]

-- Still needs an implementation
prop_7 :: Subst -> Subst -> Bool
prop_7 s1 s2 = True

prop_8 :: VarName -> VarName -> Property 
prop_8 x1 x2 = x1 /= x2 ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2]

prop_9 :: Bool 
prop_9 = allVars empty == []

prop_10 :: VarName -> Bool
prop_10 x = allVars (single x (Var x)) == []

-- Still needs the unity op in the end
prop_11 :: VarName -> Term -> Property
prop_11 x t = t /= (Var x) ==> allVars (single x t) == allVars t ++ [x]

-- Still needs an implementation
prop_12 :: Subst -> Subst -> Bool 
prop_12 s1 s2 = True

prop_13 :: VarName -> VarName -> Property 
prop_13 x1 x2 = x1 /= x2 ==> allVars (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x1, x2]

-- Still needs an implementation
prop_14 :: Subst -> Bool 
prop_14 s = True

prop_15 :: [VarName] -> Bool 
prop_15 xs = domain (restrictTo empty xs) == []

-- Still needs an implementation
prop_16 :: [VarName] -> Subst -> Bool 
prop_16 xs s = True

return []
runTests = $quickCheckAll 