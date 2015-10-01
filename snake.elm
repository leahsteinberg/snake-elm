import Keyboard exposing (arrows)
import Window
import Signal exposing (map, foldp, merge)
import Time exposing (every, second, Time)
import List exposing (head, tail, take, drop, length)
import Maybe exposing (withDefault)
import Graphics.Element exposing (image, container, middle, Element)
import Graphics.Collage exposing (collage, rect, ngon, filled, move, rotate, toForm, Form)
import Color exposing (lightRed, white, darkRed)

-- Constants
pointDim = 10
pointPlace = 15
left = {x = -1, y = 0}
right = {x = 1, y = 0}
up = {x = 0, y = 1}
down = {x = 0, y = -1}


--Models
type alias Snake = (List Dot)
type Dot = D Dir Point

type alias Point = {x: Int, y: Int}
type alias Dir = {x: Int, y: Int}--Up | Down | Left | Right


initSnake = [D left {x= -1, y =0}, D left {x=0, y= 0}, D left {x = 1, y = 0}, D down {x = 2, y =0},
           D down {x=2, y = 1}, D down {x=2, y =2}, D down {x=2, y =3}]


--Update

scooch : Dir -> Point -> Dot
scooch dir p =
  D dir {x = p.x + dir.x,  y = p.y + dir.y }




slither : Snake -> Snake
slither snake =
  let front = withDefault (D up {x=10, y=10}) (head snake)
      middle = take (length snake - 2) (withDefault ([]) (tail snake))
      back = withDefault (D up {x=0, y=0}) (head (drop (length snake - 1) snake))
  in
     case front of
       D dir p -> [scooch dir p] ++ [front] ++ middle

--moveSnake : Update -> Snake -> Snake
--move update snake = 
--  case update of
--      Tick t -> slither snake
--      Arrow dir -> turn dir snake

--turn : {x: Int, y: Int} -> Snake -> Snake
--turn dir snake = 
--  let front = withDefault (D Up {x=0, y=0}) (head snake)
--      body =  (withDefault ([]) (tail snake))
--  in
--     snake
--     case front of
--       D dir point -> [D ]




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

type Update = Tick Time | Arrow Dir

--updates : Signal Update
--updates =
--    merge
--    (map Tick (every second))
--    (map Arrow arrows)


drawSnake snake = List.map drawDot snake

display snake = collage 500 500 (drawSnake snake)
-- fold p: (a -> state -> state) -> initState -> Signal a -> Signal State
--x = foldp (\tick snake -> slither snake) initSnake (every second)
main  = map display (foldp (\change snake -> slither snake) initSnake (every second))
