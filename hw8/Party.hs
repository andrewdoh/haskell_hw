{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where
import Employee
import Data.Tree(Tree(Node))

instance Semigroup GuestList where
  (<>) (GL la fa) (GL lb fb) = GL (la ++ lb) (fa + fb)

instance Monoid GuestList where
  mempty = GL [] 0

getFun :: GuestList -> Integer
getFun (GL l f) = f

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = (GL (l ++ [e]) (f + (empFun e)))

moreFun :: GuestList -> GuestList -> GuestList
moreFun gla glb = if fa > fb then gla else glb
                                           where
                                             fa = getFun gla
                                             fb = getFun glb
                                             
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node r []) = f r []
treeFold f (Node r funs) = f r $ map (treeFold f) funs

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gl = (withBoss, withoutBoss)
  where
    withBoss = mconcat $ map fst gl
    withoutBoss = glCons e $ mconcat $ map snd gl
  
maxFun :: Tree Employee -> (GuestList, GuestList)
maxFun = treeFold nextLevel

main :: IO ()
main = putStrLn "HELLO, HASKELL!" >> readFile "company.txt" >>= (\x -> putStrLn x)
