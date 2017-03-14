module Main where

import Input
import Task1

f :: Double -> Double
f x = (x - 2) * (x - 4)

main :: IO ()
main = do
    a <- prompt "Enter left side"
    b <- prompt "Enter right side"
    putStrLn $ show $ bisectMin d f (a, b)
    where
        d = 0.000001
