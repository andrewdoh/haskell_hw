data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
 deriving (Eq, Show)



(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) lj rj = Append (lm * rm) lj rj
               where lm = tag lj
                     rm = tag rj
                     


tag :: Monoid m => JoinList m a -> m
tag Single m a = m

