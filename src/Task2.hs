{-# LANGUAGE MultiWayIf #-}

module Task2
    ( goldMin
    ) where

getU :: Floating a => a -> a -> Int -> a
getU a b i = case i of
                1 -> a + phy1 * (b - a)
                2 -> a + phy2 * (b - a)
             where
                phy1 = (3 - sqrt(5)) / 2
                phy2 = (sqrt(5) - 1) / 2

getPoints :: (Floating a, Ord b) => (a -> b) -> (a, a) -> (a, a) -> (a, a)
getPoints f (a, b) (u1, u2)
        | f1 <  f2 = (u1', u1 )
        | f1 >  f2 = (u2 , u2')
        | f1 == f2 = (u2', u1')
        where 
           f1 = f u1
           f2 = f u2
           u1' = getU a  u2 1
           u2' = getU u1 b  2

getNext :: (Floating a, Ord b) => (a -> b) -> (a, a) -> (a, a) -> (a, a)
getNext f (a, b) (u1, u2)
        | f1 <  f2 = (a,  u2)
        | f1 >  f2 = (u1, b )
        | f1 == f2 = (u1, u2)
        where 
           u = getU a b
           f1 = f $ u 1
           f2 = f $ u 2

goldHelp :: (Floating a, Ord a, Ord b) => (a -> b) -> (a, a) -> (a, a) -> a
goldHelp f (a, b) (u1, u2)
        | b - a < eps  = (a + b) / 2
        | otherwise    = goldHelp f next points
        where 
          next   = getNext   f (a, b) (u1, u2)
          points = getPoints f (a, b) (u1, u2)
          eps = 0.001


goldMin :: (Floating a, Ord a, Ord b) => (a -> b) -> (a, a) -> a
goldMin f (a, b) = goldHelp f (a, b) points
               where
                  u1 = getU a b 1
                  u2 = getU a b 2
                  points = (u1, u2)
