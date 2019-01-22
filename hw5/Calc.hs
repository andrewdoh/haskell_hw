{-# LANGUAGE FlexibleInstances #-}

import ExprT
import Parser
import qualified Data.Map as M



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
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit i = Lit i
  add x y = Add x y
  mul x y = Mul x y
 
reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit i = i
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit i = i > 0
  add x y = x || y
  mul x y = x && y

  
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)



instance Expr MinMax where
  lit i = MinMax i
  add (MinMax l)(MinMax r) = if l > r then MinMax l else MinMax r
  mul (MinMax l)(MinMax r) = if l < r then MinMax l else MinMax r

instance Expr Mod7 where
  lit i = Mod7 i
  add (Mod7 l)(Mod7 r) = Mod7 $ l + r `mod` 7
  mul (Mod7 l)(Mod7 r) = Mod7 $ l * r `mod` 7


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

class HasVars a where
  var :: String -> a

data VarExprT = L Integer
              | A VarExprT VarExprT
              | M VarExprT VarExprT
              | V String
              deriving (Show, Eq)

instance Expr VarExprT where
  lit i = L i
  add x y = A x y
  mul x y = M x y
  
instance HasVars VarExprT where
  var s = V s

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = M.lookup s 

instance Expr (M.Map String Integer -> Maybe Integer) where
 lit i   = (\_-> Just i)
 add x y = \m -> case x m of
                      Nothing     -> Nothing
                      (Just xVal) -> case y m of
                                         Nothing     -> Nothing
                                         (Just yVal) -> Just(xVal + yVal)
                                         

                    
 mul x y = \m -> case x m of
                      Nothing     -> Nothing
                      (Just xVal) -> case y m of
                                          Nothing     -> Nothing
                                          (Just yVal) -> Just(xVal * yVal)


withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


 
