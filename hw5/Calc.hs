import ExprT
import Parser




eval :: ExprT -> Integer
eval e = case e of
                 Lit i   -> i
                 Add l r -> eval l + eval r
                 Mul l r -> eval l * eval r

evalStr :: String -> Maybe Integer
evalStr s = case t of
                   Just e  -> Just $ eval e
                   Nothing -> Nothing
                   where t = parseExp Lit Add Mul s
