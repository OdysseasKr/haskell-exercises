---
--- Array operations by Odysseas (16/04/2015)
---
module ArrayOps where

--- Addition
add :: (Num a) => [a] -> [a] -> [a]
add [] [] = []
add (x:xs) (y:ys) =
    (x+y) : (add xs ys)
add _ _ = error "Invalid arguments"

--- Subtraction
sub :: (Num a) => [a] -> [a] -> [a]
sub [] [] = []
sub (x:xs) (y:ys) =
    (x-y) : (sub xs ys)
sub _ _ = error "Invalid arguments"

--- Multiplication
multi :: (Num a) => a -> [a] -> [a]
multi x y = map (*x) y

--- Division
division :: (Fractional a) => a -> [a] -> [a]
division x y = map (/x) y

--- Power
pow :: (Integral a, Num b) => a -> [b] -> [b]
pow x y = map (^x) y