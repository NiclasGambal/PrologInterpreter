module ListFunctions(nub, union, intersect,isSublistOf,isEq, intercalate) where

-- Method eliminate all duplicates in a list
nub :: Eq a => [a] -> [a]
nub li = nubHelp [] li
  where
    nubHelp retu []     = retu
    nubHelp retu (x:xs) = nubHelp (if (elem x retu) then retu else retu ++ [x]) xs

-- Method to build the union of two lists
union :: Eq a => [a] -> [a] -> [a]
union a b = a ++ (filter (\n -> not (elem n a)) b)

-- Method to build the intersect of two lists
intersect :: Eq a => [a] -> [a] -> [a]
intersect a b = filter (\n -> elem n b) a

-- Method to check if a list is a subset of another list.
isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf a b = all (\n -> elem n b) a

-- Method to check if two lists contain the same elements (like a set)
isEq :: Eq a => [a] -> [a] -> Bool
isEq a b = isSublistOf a b && isSublistOf b a

intercalate :: String -> [String] -> String
intercalate _  []     = ""
intercalate el (x:xs) = x ++ concatMap (\n -> el ++ n) xs
