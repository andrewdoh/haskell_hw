fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate (\x -> if even x && x /= 0 then x `div` 2 else if x == 1 then 0 else 3 * x  + 1)
