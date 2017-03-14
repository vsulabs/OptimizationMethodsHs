{-# LANGUAGE MultiWayIf #-}

module Lib
    ( someFunc
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

binSearch :: (Fractional a, Ord a, Ord b) => a -> (a -> b) -> (a, a) -> a
binSearch d f (a, b)
        | b - a < eps  = (a + b) / 2
        | otherwise    = binSearch d f next
        where 
          next = findNext d f (a, b)
          eps = 0.001

f :: Double -> Double
f x = (x-2)*(x-4)
          
someFunc :: IO ()
someFunc = putStrLn(show(bs f (-5.0, 5.0)))
  where 
    bs = binSearch 0.000001
