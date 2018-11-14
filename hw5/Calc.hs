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

class Expr a where
  lit :: Integer -> a
--  add :: a -> a -> b
--  mul :: a -> a -> b

instance Expr ExprT where
  lit i = Lit i
--  add a b = case a of
    --              (Add l r)  -> add l + add r
      --            _          -> a

  --mul a b = case a of
        --          (Mul l r) -> mul l * mul r
          --        Lit i       -> i
                  
                  
                 
  

  
