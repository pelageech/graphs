module MakeSVG where

import qualified Data.Text as T

type Point = (Double, Double)

concatWithN :: [T.Text] -> T.Text
concatWithN xs = T.concat $ map (`T.snoc` '\n') xs

textConcatWithN :: [String] -> T.Text
textConcatWithN xs = concatWithN $ map T.pack xs

header :: Double -> Double -> T.Text
header width height = textConcatWithN h where
      h = [
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>",
            "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"",
                  "\t\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">",
            "<svg version=\"1.1\"",
                  "\tbaseProfile=\"full\"",
                  "\txmlns=\"http://www.w3.org/2000/svg\"",
                  "\txmlns:xlink=\"http://www.w3.org/1999/xlink\"",
                  "\txmlns:ev=\"http://www.w3.org/2001/xml-events\"",
                  "\twidth=\"" ++ show width ++ "\" height=\"" ++ show height ++ "\">"
            ]

footer :: T.Text
footer = T.pack "</svg>"

(+>) :: T.Text -> T.Text -> T.Text
(+>) = T.append
infixr 0 +>

title :: String -> T.Text
title str = textConcatWithN [
      "<title>",
      "\t" ++ str ++ "</title>"
      ]

g :: (Double, Double) -> String -> T.Text -> T.Text
g translate style text = T.concat [
      textConcatWithN [
            "<g",
            "\ttransform=\"translate" ++ show translate  ++ "\"",
            "\tstyle=\"" ++ style ++ "\">"
            ],
      text,
      T.pack "</g>\n"
      ]

path :: [Char] -> T.Text
path d = textConcatWithN [
      "<path",
      "\td=\"" ++ d ++ "\" />"
      ]

text :: Point -> [Char] -> [Char] -> T.Text
text p text style = textConcatWithN [
      "<text",
      "\t x=\"" ++ show (fst p) ++ "\" y=\"" ++ show (snd p) ++ "\"",
      "\tstyle=\"" ++ style ++ "\">" ++ text ++ "</text>"
      ]

fromListText :: [(Double, Double, String)] -> String -> T.Text
fromListText xs style = foldl (\e (x, y, t) -> text (x,y) t style +> e) T.empty xs

rect :: Point -> Double -> Double -> Double -> String -> T.Text
rect p width height rx style = textConcatWithN [
      "<rect",
      "\tx=\"" ++ show (fst p) ++ "\" y=\"" ++ show (snd p) ++ "\"",
      "\twidth=\"" ++ show width ++ "\"",
      "\theight=\"" ++ show height ++ "\"",
      "\trx=\"" ++ show rx ++ "\"",
      "\tstyle=\"" ++ style ++ "\" />"
      ]

line :: Point -> Point -> String -> T.Text
line p1 p2 style = textConcatWithN [
      "<line",
      "\tx1=\"" ++ show (fst p1) ++ "\" y1=\"" ++ show (snd p1) ++ "\"",
      "\tx2=\"" ++ show (fst p2) ++ "\" y2=\"" ++ show (snd p2) ++ "\"",
      "\tstyle=\"" ++ style ++ "\" />"
      ]

fromListInterpolation :: [(Point, Point)] -> String -> T.Text -- [ ((x1 -> y1), (x2 -> y2)) ,.. ]
fromListInterpolation xs style = foldl (\e points -> T.append e (addLine points)) T.empty xs where
    addLine (p1, p2) = line p1 p2 style

circle :: Point -> Double -> String -> T.Text
circle p r style = textConcatWithN [
      "<circle",
      "\tcx=\"" ++ show (fst p) ++ "\" cy=\"" ++ show (snd p) ++ "\"",
      "\tr=\"" ++ show r ++ "\"",
      "\tstyle=\"" ++ style ++ "\" />"
      ]

createSVG :: Double -> Double -> T.Text -> T.Text
createSVG w h picture = header w h +> picture +> footer


