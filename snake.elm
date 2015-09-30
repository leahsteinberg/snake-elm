import Keyboard
import Window
import List exposing (head, tail, take, drop, length)
import Maybe exposing (withDefault)
import Graphics.Element exposing (image, container, middle, Element)
import Graphics.Collage exposing (collage, rect, ngon, filled, move, rotate, toForm, Form)
import Color exposing (lightRed, white, darkRed)

-- Constants
pointDim = 10
pointPlace = 15


--Models
type alias Snake = List Dot
type Dot = Colinear Point | Corner Dir Point
type alias Point = {x: Int, y: Int}
type Dir = Up | Down | Left | Right

theSnake = [Colinear {x=0, y= 0}, Colinear {x = 1, y = 0}, Corner Up {x = 2, y =0},
            Colinear {x=2, y = 1}, Colinear {x=2, y =2}]

turn : Dir -> Point -> Point
turn dir p = 
  case dir of
    Up ->  {p | y <- p.y + 1}
    Down -> {p | y <- p.y - 1}
    Left -> {p | x <- p.x - 1}
    Right -> {p | x <- p.x + 1}


--Update
slither : Snake -> Snake
slither snake = 
  let front = withDefault (Colinear {x=0, y=0}) (head snake)
      middle = take (length snake - 2) (withDefault ([]) (tail snake))
      back = withDefault (Colinear {x=0, y=0}) (head (drop (length snake - 1) snake))
  in
      case front of 
        Corner dir p ->
            [Colinear (turn dir p)] ++ middle
        Colinear p ->
            snake


--View

drawDot : Dot -> Form
drawDot dot = 
  let makePoint p = (toFloat p.x * pointPlace, toFloat p.y * pointPlace)
      drawPoint colour p = rect pointDim pointDim
                      |> filled colour
                      |> move (makePoint p)
  in
      case dot of 
        Colinear p ->
                drawPoint lightRed p
        Corner dir p ->
                drawPoint darkRed p


drawSnake snake = List.map drawDot snake

display snake = collage 500 500 (drawSnake snake)
  

main  = display theSnake
