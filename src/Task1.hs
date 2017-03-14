{-# LANGUAGE MultiWayIf #-}

module Task1
    ( bisectMin
    ) where

findNext :: (Fractional a, Ord b) => a -> (a -> b) -> (a, a) -> (a, a)
findNext d f (a, b) 
        | f1 <  f2 = (a,  u2)
        | f1 >  f2 = (u1, b)
        | f1 == f2 = (u1, u2)
        where 
           u1 = (a + b - d) / 2
           u2 = (a + b + d) / 2
           f1 = f u1
           f2 = f u2

bisectMin :: (Fractional a, Ord a, Ord b) => a -> (a -> b) -> (a, a) -> a
bisectMin d f (a, b)
        | b - a < eps  = (a + b) / 2
        | otherwise    = bisectMin d f next
        where 
          next = findNext d f (a, b)
          eps = 0.001
