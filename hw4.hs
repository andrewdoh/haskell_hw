data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate (\x -> if even x && x /= 0 then x `div` 2 else if x == 1 then 0 else 3 * x  + 1)

foldTree :: [a] -> Tree a
foldTree xs = foldr f Leaf xs

d :: Tree a -> Bool
d t = case t of
             (Node _ Leaf _ Leaf) -> True
	     (Node _ Leaf _ r)    -> True
	     (Node _ l _ Leaf)    -> True
	     _                    -> False
			     
f :: a -> Tree a -> Tree a
f a b = case b of
                Leaf -> (Node 0 Leaf a Leaf)
		(Node i Leaf m Leaf) -> (Node i (Node (i+1) Leaf a Leaf) m Leaf)
	        (Node i l m Leaf) -> (Node i l m (Node (i+1) Leaf a Leaf))
		(Node i Leaf m r) -> (Node i (Node (i+1) Leaf a Leaf) m r)
		(Node i l m r) -> if d b then (Node i (f a l) m r) else (Node i l m (f a r))

xor :: [Bool] -> Bool
xor xs = foldr (\x y -> x /= y) False xs
