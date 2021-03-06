import Data.List
skips :: [a] -> [[a]]
skips xs = [[fst y|y<-zip xs [1..length xs], (snd y) `mod` x == 0]| x<-[1..length xs]]


localMaxima :: [Integer] -> [Integer]
localMaxima xs = case xs of [] -> []
                            [a] -> []
                            (a:b:[]) -> []
                            (a:b:c:[]) -> if b > a && b > c then [b] else []
                            (a:b:c:dz) -> if b > a && b > c then b : localMaxima (b:c:dz) else localMaxima (b:c:dz)

histogram :: [Integer] -> String
histogram xs = unlines [[ if elem y xs then '*' else ' '| y <- [0..9]] | xs <- reverse $ transpose $ group $ sort xs] ++ "==========\n0123456789\n"
