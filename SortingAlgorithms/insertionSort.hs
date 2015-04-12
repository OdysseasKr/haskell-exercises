---
--- Insertion sort by Odysseas (12/04/2015)
---

--- Inserts n into it's correct position into a sorted list
insertInSorted :: (Ord a) => a -> [a] -> [a]
--- Alternative condition
---insertInSorted n [x]
---    | n > a = [a] ++ [n]
---    | otherwise = n:[a]

insertInSorted n [] = [n]
insertInSorted n li
    | n <= (head li) = n : li
    | otherwise = firstPart ++ insertInSorted n (secondPart)
    where (firstPart, secondPart) = splitAt 1 li

--- Sorts the given list
insertionSort :: (Ord a) => [a] -> [a]
insertionSort [x] = [x]
insertionSort (x:xs) =
    insertInSorted (x) (insertionSort xs)