data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate (\x -> if even x && x /= 0 then x `div` 2 else if x == 1 then 0 else 3 * x  + 1)

foldTree :: [a] -> Tree a
foldTree xs = foldr f Leaf xs
			     
f :: a -> Tree a -> Tree a
f a b = case b of
                Leaf -> (Node 0 Leaf a Leaf)
		(Node i Leaf m Leaf) -> (Node i (Node (i+1) Leaf a Leaf) m Leaf)
	        (Node i l m Leaf) -> (Node i l m (Node (i+1) Leaf a Leaf))
		(Node i Leaf m r) -> (Node i (Node (i+1) Leaf a Leaf) m r)
		(Node i l m r) -> if hl <= hr then (Node i (f a l) m r) else (Node i l m (f a r))
		                       where hl = height l
				             hr = height r
					     
height :: Tree a -> [Integer]
height t = case t of
                  Leaf -> [0]
		  (Node i l _ r) -> i:(height l)++(height r)
		  
xor :: [Bool] -> Bool
xor xs = foldr (\x y -> x /= y) False xs


-- mapP :: (a -> b) -> [a] -> [b]
-- mapP fa xs = foldr fa [] xs

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <-ys]

cList :: Integer -> [Integer]
cList n = [1..(2*n+2)]

--sieveSundaram :: Integer -> [Integer]
--sieveSundaram = filter(\x ->) cartProd [1..10] . cList
