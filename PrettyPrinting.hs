module PrettyPrinting (Pretty, pretty) where

import Type

class Pretty a where 
    pretty :: a -> String