{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where
import Employee
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
                                             
