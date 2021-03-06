{-# LANGUAGE MultiWayIf #-}

module Task3
    ( parabolicMethod,
      getMiddle,
      parabolaMin
    ) where

type Triples a = (a, a, a)

eps :: Fractional a => a
eps = 1e-2

getNextHelp f0 p@(f1, f2, f3) (a, b) (c, d) t1 t2
        | f0 < f2 = t1
        | f0 > f2 = t2
        | a  < b  = t1
        | c  < d  = t2
        | otherwise = p

parabolaMin :: (Fractional a) => (a -> a) -> Triples a -> a
parabolaMin f (u1, u2, u3) = u2 + num / denum
        where
          num = a * a * d1 - b * b * d2
          denum = 2 * (a * d1 + b * d2)
          a = u3 - u2
          b = u2 - u1
          d1 = f(u1) - f(u2)
          d2 = f(u3) - f(u2)

getMiddle :: (Fractional a, Ord a) => (a -> a) -> a -> Triples a -> a
getMiddle f d p@(u1, u2, u3)
          | d < eps           = u2
          | f(u2 - d) < f(u2) = u2 - d
          | f(u2 + d) < f(u2) = u2 + d
          | otherwise         = getMiddle f d2 p
          where
            d2 = d / 2

tupleMap :: (a -> b) -> Triples a -> Triples b
tupleMap f (a, b, c) = (f a, f b, f c)

getNext :: (Fractional a, Ord a) => (a -> a) -> Triples a -> Triples a
getNext f p@(u1, u2, u3)
        | d < u2    = help (f2, f1) (f3, f2) (u1, d, u2) (d,  u2, u3)
        | d > u2    = help (f2, f3) (f2, f1) (u2, d, u3) (u1, u2, d)
        | u2 == u2' = (u2, u2, u2)
        | otherwise = (u1, u2', u3)
        where
          d = parabolaMin f p
          u2' = getMiddle f delta p
          delta = (u2 - u1) / 2
          help = getNextHelp (f d) $ tupleMap f p
          (f1, f2, f3) = tupleMap f p

floatEq :: (Fractional a, Ord a) => a -> a -> a -> Bool
floatEq eps a b = t < eps
            where
              t = abs $ a - b

parabolicMethod :: (Fractional a, Ord a) => (a -> a) -> Triples a -> a
parabolicMethod f (u1, u2, u3) = 
                      if isFinish
                      then u1
                      else parabolicMethod f $ getNext f (u1, u2, u3)
                    where
                      isFinish = eq u1 u2 && eq u2 u3
                      eq = floatEq eps
