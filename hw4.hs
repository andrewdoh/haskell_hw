fun1` :: [Integer] -> Integer
fun1` = product . map (\x -> x - 2) . filter even