module Main where

import Input
import Task3

main :: IO ()
main = do
    a <- prompt "Enter left point"
    b <- prompt "Enter center point"
    c <- prompt "Enter right point"
    putStrLn $ show $ parabolicMethod f (a, b, c)
    where
--        f = \x -> (x - 2) * (x - 4)
        f = \x -> sin(x)
