import Keyboard exposing (arrows)
import Window
import Signal exposing (map, foldp, merge)
import Time exposing (every, second, Time, fps)
import List exposing (head, tail, take, drop, length, foldr)
import Maybe exposing (withDefault)
import Graphics.Element exposing (image, container, middle, Element)
import Graphics.Collage exposing (collage, rect, ngon, filled, move, rotate, toForm, Form)
import Color exposing (lightRed, white, darkRed, Color)

-- Constants
pointDimension = 10
pointPlace = 15
gridDimension = 46
left = {x = -1, y = 0}
right = {x = 1, y = 0}
up = {x = 0, y = 1}
down = {x = 0, y = -1}


--Models
type alias Snake = (List Dot)
type Dot = D Dir Point

type alias Point = {x: Int, y: Int}
type alias Dir = {x: Int, y: Int}--Up | Down | Left | Right
type alias Food = {x: Int, y: Int}
type alias State = (Food, Snake)

initFood = {x=10, y=10}

initSnake = [D left {x= -1, y =0}, D left {x=0, y= 0}, D left {x = 1, y = 0}, D down {x = 2, y =0},
           D down {x=2, y = 1}, D down {x=2, y =2}, D down {x=2, y =3}]


--Update
wrap : Int -> Int
wrap coord =
  if |coord <  -gridDimension//2 -> gridDimension//2
     |coord >= gridDimension//2 -> -gridDimension // 2
     |otherwise -> coord


getPoint : Dot -> Point
getPoint dot =
  case dot of
    D dir p -> p

getDir : Dot -> Dir
getDir dot =
  case dot of
    D dir p -> dir


checkCollide : Dot -> Snake -> Bool
checkCollide dot snake =
  let collides point1 point2 = if point1.x == point2.x && point1.y == point2.y then True else False
  in
     case dot of
       D newDot newP ->
         foldr (\snakeDot accum -> if not accum then accum else not (collides newP (getPoint snakeDot))) True snake


scooch : Dir -> Point -> Dot
scooch dir p =
  let newX = wrap (p.x + dir.x)
      newY = wrap (p.y + dir.y)
  in
      D dir {x = newX, y = newY}


slither : Snake -> Snake
slither snake =
  let front = withDefault (D up {x=10, y=10}) (head snake)
      middle = take (length snake - 2) (withDefault ([]) (tail snake))
      back = withDefault (D up {x=0, y=0}) (head (drop (length snake - 1) snake))
  in
     case front of
       D dir p ->
         let newDot = scooch dir p
         in
            if checkCollide newDot middle then [newDot] ++ [front] ++ middle else []


turn : {x: Int, y: Int} -> Snake -> Snake
turn arrowDir snake =
    let front = withDefault (D up {x=0, y=0}) (head snake)
        body =  (withDefault ([]) (tail snake))
    in
     case front of
       D dir point -> [D (noBackwards arrowDir dir) point] ++ body -- should not turn if it's the opposite direction!!!!


noBackwards : Dir -> Dir -> Dir
noBackwards arrow head =
    if |arrow.x == 0 && head.x == 0 -> head
       |arrow.y == 0 && head.y == 0 -> head
       |otherwise -> arrow

moveSnake : Update -> Snake -> Snake
moveSnake update snake =
  case update of
      Tick t -> slither snake
      Arrow dir -> turn dir snake


--View


renderPoint p = (toFloat p.x * pointPlace, toFloat p.y * pointPlace)

drawPoint : Color -> Point -> Form
drawPoint colour p = rect pointDimension pointDimension
                      |> filled colour
                      |> move (renderPoint p)



drawDot :  Color -> Dot -> Form
drawDot col dot=
      case dot of
        D dir p ->
                drawPoint col p

drawSnakeDot = drawDot lightRed

type Update = Tick Time | Arrow Dir

isDirectionArrow : Dir -> Bool
isDirectionArrow dir =
  if dir.x == 0 && dir.y == 0 then False else True


updates : Signal Update
updates =
    let
        delta = map (\t -> t/20) (fps 12)
    in
        merge
        (map Tick (every second))
        (map Arrow (Signal.filter isDirectionArrow up (Signal.sampleOn delta Keyboard.arrows)))



drawSnake snake = List.map drawSnakeDot snake

display snake = collage (gridDimension * pointPlace) (gridDimension * pointPlace) ([drawPoint darkRed initFood] ++ (drawSnake snake))
main = map display (foldp (\update snake -> moveSnake update snake) initSnake updates)





