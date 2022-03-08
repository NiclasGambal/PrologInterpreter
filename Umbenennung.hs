module Umbenennung where

import Type

import PrettyPrinting

import Vars

import Substitution
import Unifikation

import Data.List
import Test.QuickCheck

rename :: [VarName] -> Rule -> Rule
rename varnames (Rule t ts) = (Rule (apply subsToDo t) (map (\x -> (apply subsToDo x)) ts))
  where
    subsToDo = (buildSub (allVars (Rule t ts)) (filter (\n -> (not (elem n varnames))) freshVars))

  {-
  varsToRename  <- (allVars (Rule t ts))
  namesToRename <- (filter (\n -> (not (elem n varnames))) freshVars)
  subsToDo      <- (buildSub varsToRename namesToRename)
  return (Rule (apply subsToDo t) (map (\x -> (apply subsToDo x)) ts))
-}


buildSub :: [VarName] -> [VarName] -> Subst
buildSub (x:xs) (y:ys) = compose (single x (Var y)) (buildSub xs ys)
buildSub _      _      = empty
--
