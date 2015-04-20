---
--- Matrix operations by Odysseas (20/04/2015)
---
module Linear.MatrixOps
( Linear.MatrixOps.add
, Linear.MatrixOps.sub
, Linear.MatrixOps.multi
, isMatrix
, areMultipliable
) where

import Linear.ArrayOps as ArrayOps

--- Addition
add :: (Num a) => [[a]] -> [[a]] -> [[a]]
add [] [] = []
add (x:xs) (y:ys) =
    (ArrayOps.add x y) : Linear.MatrixOps.add xs ys
add _ _ = error "Invalid arguments"

--- Subtraction
sub :: (Num a) => [[a]] -> [[a]] -> [[a]]
sub [] [] = []
sub (x:xs) (y:ys) =
    (ArrayOps.sub x y) : Linear.MatrixOps.sub xs ys
sub _ _ = error "Invalid arguments"

--- Multiplication
multi :: (Num a) => [[a]] -> [[a]] -> [[a]]
multi [] _ = []
multi (x:xs) ys = 
    (calculateRow x ys) : (Linear.MatrixOps.multi xs ys)
    where
        calculateRow :: (Num a) => [a] -> [[a]] -> [a]
        calculateRow _ ([]:xs) = []
        calculateRow x ys = 
            sum (ArrayOps.elMulti (map (head) ys) x) : (calculateRow x (map (tail) ys))

--- Checks if the given lists of lists is a matrix
isMatrix :: (Num a) => [[a]] -> Bool
isMatrix (x:[]) = True
isMatrix (x:y:matrix) =
    (length x == length y) && isMatrix (y:matrix)

--- Checks if the two given matrixes can be multiplied
areMultipliable :: (Num a) => [[a]] -> [[a]] -> Bool
areMultipliable (x:xs) (y:ys) = 
    (length (x:xs) == length y) && (length x == length (y:ys))