fib :: Integer -> Integer
fib n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]



data Stream a = C a deriving Show

streamToList :: Stream a -> [a]
streamToList s = ?

