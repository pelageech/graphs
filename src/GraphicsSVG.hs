module GraphicsSVG (Graph (..), allFunction) where

import MakeSVG ( (+>), circle, fromListInterpolation, fromListText, g, rect )
import FunctionSVG ( evaluate, Expr )

import Data.Maybe ( fromJust )
import Data.Fixed ( mod', div' )
import qualified Data.Text as T

type Point = (Double, Double)

data Graph = Graph {
    function :: Expr,
    xstart :: Double, ystart :: Double,
    width :: Double, height :: Double,
    segment :: Double,
    xoffset :: Double, yoffset :: Double
} deriving Show

-- Объединение координатной системы и графика
allFunction :: Graph -> T.Text
allFunction grph = g (xstart grph, ystart grph) "stroke:black"
                  ( drawFunction grph
                 +> background grph "stroke-width: 4; fill: none"
                 +> setCoordinateSystem grph
                 +> center grph )


-- Задать задний фон для графика
background :: Graph -> String -> T.Text
background grph style = rect (0, 0) (width grph) (height grph) 4 style


-- Отмечает начало координат красной точкой на экране
center :: Graph -> T.Text
center grph = circle ( -xoffsetseg + (width grph / 2), yoffsetseg + (height grph / 2) ) 5 style where
  
  -- Смещение на единичные отрезки
    xoffsetseg = xoffset grph * segment grph
    yoffsetseg = yoffset grph * segment grph

    style | xoffsetseg > width grph / 2 || yoffsetseg > height grph / 2 = "opacity:0"
          | otherwise = "fill: red"


-- Установить размер окна и начертить систему координат (точка О в центре окна) с единичным отрезков segment grph (всё в пикселях)
setCoordinateSystem :: Graph -> T.Text
setCoordinateSystem grph = lineX +> numsX +> lineY +> numsY where

  -- Смещение числовых точек на координатных лучах
    xoffset' = xoffsetseg `mod'` segment grph
    yoffset' = yoffsetseg `mod'` segment grph

  -- Смещение на единичные отрезки
    xoffsetseg = xoffset grph * segment grph
    yoffsetseg = yoffset grph * segment grph

  -- Координаты на луче Ох (Left - влево от центра экрана, Right - вправо)
    xPointsLeft  = [ width grph / 2 - xoffset', width grph / 2 - xoffset' - segment grph .. 0 ]
    xPointsRight = [ width grph / 2 + segment grph - xoffset', width grph / 2 + (2 * segment grph) - xoffset' .. width grph ]

  -- Координаты на луче Оy (Up - вверх от центра экрана, Down - вниз)
    yPointsUp   = [ height grph / 2 + yoffset', height grph / 2 + segment grph + yoffset'..height grph ]
    yPointsDown = [ height grph / 2 - segment grph + yoffset', height grph / 2 - (2 * segment grph) + yoffset' .. 0 ]

  -- Точки обозначения координат по оси Ох
    lineX = fromListInterpolation (points (height grph) 0) "stroke-width: 1; stroke: black; opacity: 0.3" +>
            fromListInterpolation (points (height grph) (height grph+10)) "stroke-width: 3" where

        points yFrom yTo = [ ((x, yFrom), (x, yTo)) |
            x <- xPointsLeft, x >= 0 ]
              ++ [ ((x, yFrom), (x, yTo)) |
            x <- xPointsRight, x <= width grph ]

  -- Числа на оси Ох
    numsX = fromListText (txt $ height grph + 24) style where
        style = "font: Lobster; stroke-width: 0; fill: black; text-anchor: middle"

        txt y = zip3 [x | x <- xPointsLeft, x >= 0 ]
                     (repeat y)
                     (map show [xoffsetseg `div'` segment grph, xoffsetseg `div'` segment grph - 1..])
             ++ zip3 [x | x <- xPointsRight, x <= width grph ]
                     (repeat y)
                     (map show [xoffsetseg `div'` segment grph + 1, xoffsetseg `div'` segment grph + 2..])


  -- Точки обозначения координат по оси Оу
    lineY = fromListInterpolation (points 0 (width grph)) "stroke-width: 1; stroke: black; opacity: 0.3" +>
            fromListInterpolation (points 0 (-10)) "stroke-width: 3" where

        points xFrom xTo = [ ((xFrom, y), (xTo, y)) |
                    y <- yPointsUp, y <= height grph ]
                        ++ [ ((xFrom, y), (xTo, y)) |
                    y <- yPointsDown, y >= 0 ]

  -- Числа на оси Оу
    numsY = g (0, 0) "" (fromListText (txt $ -12) style) where
        style = "font: Lobster; stroke-width: 0; fill: black; text-anchor: end"

        txt x = zip3 (repeat x)
                     [ y + 5 | y <- yPointsUp, y <= height grph ]
                     (map show [ yoffsetseg `div'` segment grph, yoffsetseg `div'` segment grph - 1..])
             ++ zip3 (repeat x)
                     [ y + 5 | y <- yPointsDown, y >= 0 ]
                     (map show [ yoffsetseg `div'` segment grph + 1, yoffsetseg `div'` segment grph + 2..])


-- Сама функция
drawFunction :: Graph -> T.Text
drawFunction grph = fromListInterpolation xy "stroke-width: 2; stroke: red" where
  -- Вершины отрезков линейной интерполяции
    xs = [ x | x <- [0, 650 * width grph / 4029 / height grph..width grph], 0 <= x && x <= width grph ]

  -- Смещение на единичные отрезки
    xoffsetseg = xoffset grph * segment grph
    yoffsetseg = yoffset grph * segment grph

    ys = [(\y
              -> if y > height grph
                    then   height grph
                    else   if y < 0 then 0 else y )
        (fromJust
            $ (+ ( height grph / 2 + yoffsetseg ))
                <$> ((* (- segment grph)) <$> evaluate x (function grph))) |
        x <- map
              (\ x -> (x - width grph / 2 + xoffsetseg) / segment grph) xs]

    xy1 = zip xs ys
    xy2 = zip (tail xs) (map (\y -> if y >= 4e15 then -y else y) (tail ys))
    xy = filter (
                  \ (xy1, xy2) -> 
                    abs (snd xy1 - snd xy2) < height grph
                ) (zip xy1 xy2)
