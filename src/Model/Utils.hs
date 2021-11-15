module Model.Utils where

-- | Create list of tuples from a given list
-- When the list contains only one element, it will be on the two side of the tuple
listToTuples :: [a] -> [(a, a)]
listToTuples [] = []
listToTuples [x] = [(x, x)]
listToTuples (x : y : zs) = (x, y) : listToTuples zs
