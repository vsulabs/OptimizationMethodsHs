module Main where

import Lib

f :: Double -> Double
f x = (x - 2) * (x - 4)

main :: IO ()
main = putStrLn $ show $ bisectMin d f interval
    where
        d = 0.000001
        a = -5.0
        b = 5.0
        interval = (a, b)
