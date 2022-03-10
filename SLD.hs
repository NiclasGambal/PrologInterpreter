module SLD where

import Type

import Vars
import Umbenennung
import Substitution

import Unifikation
import PrettyPrinting

data SLDTree = Node Goal [(Subst, SLDTree)]
    deriving Show


sld :: Prog -> Goal -> SLDTree
sld _            (Goal []) = Node (Goal []) []
-- versuche, das Regelset auf jeden Term anzuwenden
sld (Prog rules) (Goal ts) = Node (Goal ts) (concatMap (\oneTerm -> (concatMap (\oneRule -> (tryRule (Prog rules) (Goal ts) (rename ((allVars (Prog rules)) ++ (allVars (Goal ts))) oneRule) oneTerm)) rules)) ts)

--sldRename :: [VarName] -> Prog -> Goal -> SLDTree
--sldRename vn (Prog rs) g = sldRenamed vn (Prog (map (\r -> (rename vn r)) rs)) g

--sldRenamed :: [VarName] -> Prog -> Goal -> SLDTree
--sldRenamed _  _            (Goal []) = Node (Goal []) []
-- versuche, das Regelset auf jeden Term anzuwenden
--sldRenamed vn (Prog rules) (Goal ts) = Node (Goal ts) (concatMap (\oneTerm -> (concatMap (\oneRule -> (tryRule vn (Prog rules) (Goal ts) oneRule oneTerm)) rules)) ts)

--tryRules :: [Rule] -> Goal -> Term -> [(Subst, SLDTree)]
--tryRules []     _  _ = []
--tryRules (r:rs) ts t = [(empty, (Node (Goal [t]) []))] ++ tryRules rs ts t

tryRule:: Prog -> Goal -> Rule -> Term -> [(Subst, SLDTree)]
tryRule p (Goal ts) (Rule rHead rTail) t = if ((unify rHead t) == Nothing) then [] else [(dontBeAMaybeSubst (unify rHead t), sld p (Goal ((filter (/= t) ts) ++ (map (apply (dontBeAMaybeSubst (unify rHead t))) rTail))))]
--(sld p (Goal ((filter (/= t) ts) ++ (map (apply (dontBeAMaybeSubst (unify rHead t))) rTail))) ))]

--tryRule :: Rule -> [Term] -> Term -> [(Subst, SLDTree)]
--tryRule (Rule rHead rTail) t = if (unify rHead t) == Nothing then [] else [(dontBeAMaybeSubst (unify rHead t), )]


-- Method to take the Substitution out of a Maybe Substitution
dontBeAMaybeSubst :: Maybe Subst -> Subst
dontBeAMaybeSubst Nothing    = empty
dontBeAMaybeSubst (Just sub) = sub
