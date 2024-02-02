module Block where

data Block = Air | Dirt | Stone

gen :: Int -> Int -> Int -> Block
gen x y z
    | y < x + z  = Stone
    | y == x + z = Dirt
    | otherwise  = Air

