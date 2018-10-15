


toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 1 = []
              | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n | n <= 1 = []
           | otherwise = n `div` (10 ^ (numOfDigits $ n)) : toDigits (n `mod` (10 ^(numOfDigits $ n)))


numOfDigits :: Integer -> Integer
numOfDigits 0 = -1
numOfDigits n = 1 + (numOfDigits $ n `div` 10)



doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x: xs) = if lenOfList xs `mod` 2 /= 0 then (x *2) : doubleEveryOther xs else x : doubleEveryOther xs  



lenOfList :: [a] -> Integer
lenOfList [] = 0
lenOfList (x:xs) = 1 + lenOfList xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = if x `div` 10 == 1 then (x `div` 10 + x `mod` 10) + sumDigits xs else x + sumDigits xs

validate :: Integer -> Bool
validate n = mod (sumDigits $ doubleEveryOther $ toDigits n) 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = 





















