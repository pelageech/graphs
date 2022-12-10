module Main where

-- import Control.Monad
-- import Control.Monad.Trans.Either
import MakeSVG ( createSVG )
import GraphicsSVG
    ( allFunction, Graph(segment, width, height, xstart, ystart) )

import qualified Data.Text as T
import Data.Text.IO as T.IO ( putStr )

import ParserSVG ( parse )

main :: IO ()
main = parse >>= checkedValues



checkedValues :: Graph -> IO ()
checkedValues graph | segment graph > 500 || segment graph < 20                   = print "Error: The segment must be between 20 and 500 pixels"
                    | width graph < segment graph || height graph < segment graph = print "Error: Wrong window size: width and height must be greater than a single line segment"
                    | width graph > 5000 || height graph > 3000                   = print "Error: Wrong window size: width and height must be less than 5000"
                    | xstart graph < 0 || ystart graph < 0                        = print "Error: Screen start point set incorrectly"
                    | otherwise                                                   = T.IO.putStr (createSVG w h $ allFunction graph) where
                                                                                        w = width graph + 2 * xstart graph
                                                                                        h = height graph + 2 * ystart graph
