{-# LANGUAGE MultiWayIf #-}

module Lib
    ( someFunc
    ) where

findNext :: Double -> (Double -> Double) -> Double -> Double -> (Double, Double)
findNext d f a b 
        | f1 <  f2 = (a,  u2)
        | f1 >  f2 = (u1, b)
        | f1 == f2 = (u1, u2)
        where 
           u1 = (a + b - d) / 2
           u2 = (a + b + d) / 2
           f1 = f u1
           f2 = f u2

binSearch :: Double -> (Double -> Double) -> Double -> Double -> Double
binSearch d f a b
        | b - a < eps  = (a + b) / 2
        | otherwise    = binSearch d f a1 b1
        where 
          (a1, b1) = findNext d f a b
          eps = 0.001

f :: Double -> Double
f x = (x-2)*(x-4)

someFunc :: IO ()

someFunc = putStrLn(show(bs f (-5.0) 5.0))
  where bs = binSearch 0.000001
