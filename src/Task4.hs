{-# LANGUAGE MultiWayIf #-}

module Task4
    ( newton,
      getNext,
      applyM,
      apply, 
      norm
    ) where

import Data.Matrix

eps :: Fractional a => a
eps = 1e-2

norm :: (Floating a) => [a] -> a
norm v = sqrt $ squareSum v
      where
        squareSum [] = 0
        squareSum (x:xs) = x * x + squareSum xs

apply :: (Floating a) => [a -> a] -> a -> [a]
apply [] x = []
apply (b:bs) x = (b x : apply bs x)

applyM :: (Floating a) => Matrix (a -> a) -> a -> Matrix a
applyM m x = fromList r c $ (apply l x)
          where
             r = nrows m
             c = ncols m
             l = toList m

getNext :: (Num a) => [a] -> Matrix a -> a -> a
getNext grad h x0 = x0 - getElem 1 1 m
                where
                  g = fromList 1 l grad
                  l = length grad
                  m = g * transpose h

newton :: (Floating a, Ord a) => [a -> a] -> Matrix (a -> a) -> a -> a
newton grad h x0 = 
                if norm g < eps
                then x0
                else newton grad h x1
              where
                g = apply grad x0
                m = applyM h x0
                x1 = getNext g m x0
