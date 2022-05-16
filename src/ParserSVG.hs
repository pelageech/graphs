module ParserSVG ( parse ) where

import GraphicsSVG ( Graph (..) )
import Options.Applicative
    ( Parser,
      helper,
      ParserInfo,
      argument,
      auto,
      fullDesc,
      header,
      help,
      hidden,
      info,
      infoOption,
      long,
      metavar,
      option,
      progDesc,
      short,
      style,
      value,
      execParser )
import Options.Applicative.Help ( bold )
import qualified Control.Applicative as Options

parse :: IO Graph
parse = execParser graphParser where

    graphParser :: ParserInfo Graph
    graphParser = info
        (helper <*> programOptions )
        (fullDesc <> progDesc "A small program which draw you any functions! Point --help for more information" <>
            header
                "Welcome to Graphics!" )

    versionOption :: Parser (a -> a)
    versionOption = infoOption "Graphics | ver 0.0.1.0" (help "Show version" <> hidden)

    programOptions :: Parser Graph
    programOptions =
        Graph <$> argument auto (help "Set a function: (Num, X, Add, Sub, Mul, Div and trigonomic)" <> style bold <> metavar "FUNCTION") <*>
                    option auto (long "xstart" <> value 40 <> help "Set x-start of the window (default: 40)") <*>
                    option auto (long "ystart" <> value 40 <> help "Set y-start of the window (default: 40)") <*>
                    option auto (long "width" <> value 1455 <> help "Set width of the window (default: 1455)") <*>
                    option auto (long "height" <> value 650 <> help "Set height of the window (default: 650)") <*>
                    option auto (long "segment" <> short 's' <> value 44 <> help "Set a single segment of the graph (default: 44, almost 1cm)") <*>
                    option auto (long "xoffset" <> short 'x' <> value 0 <> help "Set xoffset relative to the origin") <*>
                    option auto (long "yoffset" <> short 'y' <> value 0 <> help "Set yoffset relative to the origin") 