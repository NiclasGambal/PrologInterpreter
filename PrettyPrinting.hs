module PrettyPrinting (Pretty, pretty) where

import Type
import ListFunctions

class Pretty a where
    pretty :: a -> String

instance Pretty Term where
    -- Simple gets the name of the variable.
    pretty (Var (VarName a)) = a
    -- Case that there is only the functor for the term
    pretty (Comb n []) = n
    -- Case that the term is made out of multiple terms, using intercalate to integrate the ", " between the terms.
    pretty (Comb n ts) = n ++ "(" ++ intercalate ", " (map (\t -> pretty t) ts) ++ ")"

instance Pretty Rule where
    -- Basic case with no extra terms coming with that rule.
    pretty (Rule n []) = pretty n ++ "."
    -- Case for extra terms, describing the rule.
    pretty (Rule n ts) = pretty n ++ " :- " ++ intercalate ", " (map (\t -> pretty t) ts) ++ "."

instance Pretty Prog where
    -- Same pattern as in the upper examples.
    pretty (Prog rs)  = intercalate "\n" (map (\r -> pretty r) rs)

instance Pretty Goal where
    -- Suprise! Again the same pattern.
    pretty (Goal ts)  = "?- " ++ intercalate ", " (map (\t -> pretty t) ts) ++ "."
