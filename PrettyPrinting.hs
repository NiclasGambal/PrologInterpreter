module PrettyPrinting (Pretty, pretty) where

import Type

class Pretty a where
    pretty :: a -> String

instance Pretty Term where
    -- Simple gets the name of the variable.
    pretty (Var (VarName a)) = a
    -- Case that there is only the name for the combinator
    pretty (Comb n []) = n
    -- Makes no sense, guck ich morgen an xD
    pretty (Comb n (t:ts))    = n ++ "(" ++ (pretty t) ++ (rest ts)
        where rest []     = ")"
              rest (x:xs) = ", " ++ (pretty x) ++ (rest xs)

instance Pretty Rule where
    pretty (Rule n []) = (pretty n) ++ "."
    pretty (Rule n (t:ts)) = (pretty n) ++ " :- " ++ (pretty t) ++ (rest ts)
        where rest []     = "."
              rest (x:xs) = ", " ++ (pretty x) ++ (rest xs)

instance Pretty Prog where
    pretty (Prog []) = ""

instance Pretty Goal where
    pretty (Goal []) = "?- ."