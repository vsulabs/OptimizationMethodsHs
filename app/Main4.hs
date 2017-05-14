module Main where

import Input
import Task4
import Data.Matrix

main :: IO ()
main = do
    a <- prompt "Enter point"
    putStrLn $ show $ newton grad h a
    where
        grad = [\x -> x*x, \x -> 2*x]
        h = fromLists [ [const 2, const 1], 
                        [const 1, const 2] ]
