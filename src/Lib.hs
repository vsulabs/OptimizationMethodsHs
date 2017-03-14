{-# LANGUAGE MultiWayIf #-}

module Lib
    ( someFunc
    ) where

data Pair = Pair Double Double

findNext :: Double -> (Double -> Double) -> Double -> Double -> Pair
findNext d f a b = if | f1 <  f2 -> Pair a u2
                      | f1 >  f2 -> Pair u1 b
                      | f1 == f2 -> Pair u1 u2
                   where 
                      u1 = (a + b - d) / 2
                      u2 = (a + b + d) / 2
                      f1 = f u1
                      f2 = f u2

binSearch :: Double -> (Double -> Double) -> Double -> Double -> Double
binSearch d f a b =   if b - a < eps 
                      then (a + b) / 2
                      else binSearch d f a1 b1
                    where 
                      Pair a1 b1 = findNext d f a b
                      eps = 0.001

f :: Double -> Double
f x = (x-2)*(x-4)

someFunc :: IO ()

someFunc = putStrLn(show(bs f (-5.0) 5.0))
  where bs = binSearch 0.000001
