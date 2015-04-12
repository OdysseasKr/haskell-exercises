---
--- Selection sort by Odysseas (12/04/2015)
---

--- Removes the given item from the given list
removeItem :: (Ord a) => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys) 
    | x == y    = ys
    | otherwise = y : removeItem x ys

--- Sorts the given list
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [x] = [x]
selectionSort xs = 
    mn : (selectionSort (removeItem mn xs))
    where mn = minimum xs