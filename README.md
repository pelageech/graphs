# graphics

My haskell project. You can draw gifferent function to SVG-file

## How to use it?

First you need <a href="https://docs.haskellstack.org/en/stable/README/">Haskell Stack</a>.
Clone this repo and run `stack build` the created folder.

You can find the application in `.\graphics\.stack-work\dist\d53b6a14\build\Graphics-exe`

Run `./Graphics-exe.exe "FUNCTION"` (for example, `./Graphics-exe.exe "Add X (Sin X)"`). SVG-file will be created in the same folder where the program was launched from.

## Function

Building a function:
- **Num**  _14_ - a number
- **X** - an argument
- **Add/Sub/Mul/Div**  ( _Expr_ ) ( _Expr_ )
- **Pow**  ( _Expr_ ) ( _Expr_ ) - Expr^Expr
- **Sin/Cos/Tan**  ( _Expr_ )

_Example_: **Mul ( Sin ( Pow X ( Num 2 ) ) ) ( Add ( Cos X ) ( Div ( Sin X ) X ) )**  - sin(X^2) \* (cosX + sinX / X)

#### The result will be: 
<img style="width: 60%" src="https://i.imgur.com/zcaGOzn.png">

## Parameters
All the parameters in pixels
-  `--xstart ARG` Set x-start of the window (default: 40)
-  `--ystart ARG` Set y-start of the window (default: 40)
-  `--width ARG` Set width of the window (default: 1455)
-  `--height ARG` Set height of the window (default: 650)
-  `-s` `--segment ARG` Set a single segment of the graph (default: 44, almost 1cm)
-  `-x` `--xoffset ARG` Set xoffset relative to the origin
-  `-y` `--yoffset ARG` Set yoffset relative to the origin
