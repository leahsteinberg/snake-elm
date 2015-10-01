import Keyboard
import Window
import Signal exposing (map, foldp)
import Time exposing (every, second)
import List exposing (head, tail, take, drop, length)
import Maybe exposing (withDefault)
import Graphics.Element exposing (image, container, middle, Element)
import Graphics.Collage exposing (collage, rect, ngon, filled, move, rotate, toForm, Form)
import Color exposing (lightRed, white, darkRed)

-- Constants
pointDim = 10
pointPlace = 15


--Models
type alias Snake = (List Dot)
type Dot = D Dir Point

type alias Point = {x: Int, y: Int}
type Dir = Up | Down | Left | Right

initSnake = [D Left {x= -1, y =0}, D Left {x=0, y= 0}, D Left {x = 1, y = 0}, D Down {x = 2, y =0},
           D Down {x=2, y = 1}, D Down {x=2, y =2}, D Down {x=2, y =3}]


--Update
scooch : Dir -> Point -> Dot
scooch dir p = 
  case dir of
    Up -> D dir {p | y <- p.y + 1}
    Down -> D dir {p | y <- p.y - 1}
    Left -> D dir {p | x <- p.x - 1}
    Right -> D dir {p | x <- p.x + 1}



slither : Snake -> Snake
slither snake = 
  let front = withDefault (D Up {x=0, y=0}) (head snake)
      middle = take (length snake - 2) (withDefault ([]) (tail snake))
      back = withDefault (D Up {x=0, y=0}) (head (drop (length snake - 1) snake))
  in
     case front of
       D dir p -> [scooch dir p] ++ [front] ++ middle


--View

drawDot : Dot -> Form
drawDot dot = 
  let makePoint p = (toFloat p.x * pointPlace, toFloat p.y * pointPlace)
      drawPoint colour p = rect pointDim pointDim
                      |> filled colour
                      |> move (makePoint p)
  in
      case dot of 
        D dir p ->
                drawPoint lightRed p


drawSnake snake = List.map drawDot snake

display snake = collage 500 500 (drawSnake snake)
-- fold p: (a -> state -> state) -> initState -> Signal a -> Signal State
x = foldp (\tick snake -> slither snake) initSnake (every second)
main  = map display (foldp (\tick snake -> slither snake) initSnake (every second))
