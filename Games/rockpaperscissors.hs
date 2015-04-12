---
--- Rock-Paper-Scissors-Shoot by Odysseas (11/04/2015)
---

--- Call this to play a round
game :: [Char] -> [Char] -> Int
game p1 p2
    | p1 == p2 = 0 
    | p1 == "rock" =  (rock p2)
    | p1 == "paper" =  (paper p2)
    | p1 == "sci" =  (sci p2)
    where
	rock :: [Char] -> Int
	rock m
    	     | m == "sci" =  1
    	     | m == "paper" =  2

	paper :: [Char] -> Int
	paper m
    	     | m == "sci" =  2
    	     | m == "rock" =  1

        sci :: [Char] -> Int
        sci m
            | m == "rock" =  2
            | m == "paper" =  1
