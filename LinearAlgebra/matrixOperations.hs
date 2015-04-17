module MatrixOps
( MatrixOps.add
, MatrixOps.sub
, MatrixOps.multi
) where

import Linear.ArrayOps as ArrayOps

--- Addition
add :: (Num a) => [[a]] -> [[a]] -> [[a]]
add [] [] = []
add (x:xs) (y:ys) =
    (ArrayOps.add x y) : MatrixOps.add xs ys
add _ _ = error "Invalid arguments"

--- Subtraction
sub :: (Num a) => [[a]] -> [[a]] -> [[a]]
sub [] [] = []
sub (x:xs) (y:ys) =
    (ArrayOps.sub x y) : MatrixOps.sub xs ys
sub _ _ = error "Invalid arguments"