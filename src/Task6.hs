module Task6
    ( task6
    ) where

import Data.Bool
import Text.Printf

type Point = (Double, Double)

task6 :: IO ()
task6
    | snd result    = printf "Минимум найден в точке (%.4f, %.4f)\n" x y
    | otherwise     = printf "Минимум не найден, последняя найденная точка: (%.4f, %.4f)\n" x y
    where
        result  = stepDivision (103.0, 34.0) 0.000001 10000 1
        (x, y) = fst result

f (x, y) = (x - 4) ^ 2 + (y + 7) ^ 2 - 4

f_der_x x y = 2 * x - 8

f_der_y x y = 2 * y + 14

gradient (x, y) = (f_der_x x y, f_der_y x y)

measure (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

getNextPointAndStep currentPoint currentStep
        | (f nextPoint) <= (f currentPoint) - nextStep * gradientLength ^ 2 = (nextPoint, nextStep)
        | otherwise                                                         = getNextPointAndStep currentPoint nextStep
        where
            x = fst currentPoint 
            y = snd currentPoint
            (gradX, gradY) = gradient currentPoint
            gradientLength = measure (0, 0) (gradX, gradY)
            nextPoint = (x - currentStep * gradX, y - currentStep * gradY)
            nextStep = currentStep / 2;

stepDivision :: Point -> Double -> Integer -> Double -> (Point, Bool)
stepDivision startPoint eps iterCount step
        | iterCount == 0                        = (startPoint, False)
        | measure startPoint nextPoint < eps    = (nextPoint, True)
        | otherwise                             = stepDivision nextPoint eps (iterCount - 1) nextStep
        where
            (nextPoint, nextStep) = getNextPointAndStep startPoint step
