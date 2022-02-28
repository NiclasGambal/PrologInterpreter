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
    pretty (Comb n [Var (VarName a)]) = n ++ "(" ++ a ++")"

instance Pretty Rule where
    pretty (Rule t []) = pretty t ++ "."
    pretty (Rule t ts) = pretty t ++ " :- "

instance Pretty Prog where
    pretty (Prog rs) = "Si"

instance Pretty Goal where
    pretty (Goal ts) = "Yes"