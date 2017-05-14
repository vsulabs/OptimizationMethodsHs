module Main where

import Input
import Task3

e :: Double
e = 2.7

main :: IO ()
main = do
    a <- prompt "Enter left point"
    b <- prompt "Enter center point"
    c <- prompt "Enter right point"
    putStrLn $ show $ parabolicMethod f (a, b, c)
    where
--        f = \x -> (x - 2) * (x - 4)
--        Параболоид a*x^2 + b*y^2 + c xy + d = f(x,y)
        f = \x -> (e ** x + e ** (-x)) / 2
