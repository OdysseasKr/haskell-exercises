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

--- Multiplication
multi :: (Num a) => [[a]] -> [[a]] -> [[a]]
multi [] _ = []
multi (x:xs) ys = 
    (computeRow x ys) : MatrixOps.multi xs ys
    where 
        yrow = map (head) ys
        computeRow _ _ = [1,1,1] 
multi _ _ = error "Invalid arguments"