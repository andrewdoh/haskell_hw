import Sized
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
 deriving (Eq, Show)



(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) lj rj = Append (lm <> rm) lj rj
               where lm = tag lj
                     rm = tag rj


tag :: Monoid m => JoinList m a -> m
tag jl = case jl of
           Empty -> mempty 
           (Single m _ ) -> m
           (Append m _ _ ) -> m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl = case jl of
                 Empty -> Nothing



