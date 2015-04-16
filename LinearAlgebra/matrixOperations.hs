import ArrayOps

module MatrixOps where

--- Addition
add :: (Num a) => [[a]] -> [[a]] -> [[a]]
add [] [] = []
add (x:xs) (y:ys) =
    (ArrayOps.add x y) : Main.add xs ys
add _ _ = error "Invalid arguments"

--- Subtraction
sub :: (Num a) => [[a]] -> [[a]] -> [[a]]
sub [] [] = []
sub (x:xs) (y:ys) =
    (ArrayOps.sub x y) : Main.sub xs ys
sub _ _ = error "Invalid arguments"