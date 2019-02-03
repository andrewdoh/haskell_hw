{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where
import Data.Monoid
import Data.Semigroup

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

class Scored a where
  scored :: a -> Score

instance Scored Score where
  scored = id
  
instance Scored a => Scored (a, b) where
  scored = scored . fst
        
getScore :: Score -> Int
getScore (Score s) = s
  
score :: Char -> Score
score c = case c of
             'a' -> Score 1
             'b' -> Score 3
             'c' -> Score 3
             'd' -> Score 2
             'e' -> Score 1
             'f' -> Score 4
             'g' -> Score 2
             'h' -> Score 4
             'i' -> Score 1
             'j' -> Score 8
             'k' -> Score 5
             'l' -> Score 1
             'm' -> Score 3
             'n' -> Score 1
             'o' -> Score 1
             'p' -> Score 3
             'q' -> Score 10
             'r' -> Score 1
             's' -> Score 1
             't' -> Score 1
             'u' -> Score 1
             'v' -> Score 4
             'w' -> Score 4
             'x' -> Score 8
             'y' -> Score 4
             'z' -> Score 10
             _   -> Score 0



scoreString :: String -> Score
scoreString = foldl (<>) (Score 0) . map score

