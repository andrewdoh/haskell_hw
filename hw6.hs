fib :: Integer -> Integer
fib n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]



data Stream a =  C a (Stream a) deriving Show

streamToList :: Stream a -> [a]
streamToList (C a b) = a : streamToList b


streamRepeat :: a -> Stream a
streamRepeat x = C x (streamRepeat x)

--streamMap :: (a -> b) -> Stream a -> Stream b
--streamMap f (C a b) = C $ (f a) streamMap b


