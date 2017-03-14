module Input
    ( prompt
    ) where

readDouble :: String -> Double
readDouble = read

prompt x = do
       putStrLn x
       number <- getLine
       return(readDouble number)
