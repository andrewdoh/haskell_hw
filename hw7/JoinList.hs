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
indexJ i jl
            | i >= getSize (size $ tag jl) || i < 0 = Nothing
            | otherwise            = case jl of
                                         Empty -> Nothing
                                         (Single m a) -> Just a
                                         (Append _ l r) -> if i < lSize then indexJ i l else indexJ (i - lSize) r
                                                                       where
                                                                           lSize = getSize $ size $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl
           | i > (getSize (size $ tag jl)) || i < 1 = jl
           | otherwise                      = case jl of
                                                Empty -> Empty
                                                (Single m _ ) -> Empty
                                                (Append m l r) -> if i <= lSize then left else right
                                                                   where
                                                                         lSize = getSize $ size $ tag l
                                                                         rSize = getSize $ size $ tag r
                                                                         nm    = tag l <> tag r
                                                                         left  = Append nm (dropJ i l) r
                                                                         right = Append nm Empty (dropJ (i - lSize) r)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i jl = jl
                                                                         


