import Keyboard exposing (arrows)
import Window
import Signal exposing (map, foldp, merge, filter, sampleOn)
import Time exposing (every, second, Time, fps)
import List exposing (head, tail, take, drop, length, foldr)
import Maybe exposing (withDefault)
import Graphics.Element exposing (image, container, middle, Element)
import Graphics.Collage exposing (collage, rect, ngon, filled, move, rotate, toForm, Form)
import Color exposing (lightRed, white, darkRed, Color)


-- Model

type alias Snake = List (Vertebra)
type alias Vertebra = {point: Point, dir: Dir}
type alias Point = {x: Int, y: Int}
type alias Dir = {x: Int, y: Int}
type alias Food = Point
type alias State = {food: Food, snake: Snake}
  
type Update = Tick Time | Arrow Dir

pointDim = 10
pointOffset = 15
gridDimension = 46
defaultVert = {point= {x=0, y=0}, dir: {x=1, y =0}}
initFood = {x= 5, y = 5}

initSnake = [
  {point= {x= -1, y=0}, dir= {x= -1, y= 0}}
  ,{point= {x= 0, y=0}, dir= {x= -1, y= 0}}
  ,{point= {x= 1, y=0}, dir= {x= -1, y= 0}}
  ,{point= {x= 1, y:=1}, dir= {x= 0, y= -1}}
  ,{point= {x= 1, y=2}, dir= {x= 0, y= -1}}
  ,{point= {x= 1, y=3}, dir= {x= 0, y= -1}}
  ,{point= {x= 2, y=3}, dir= {x= -1, y= 0}}
]

initState = (initFood, initSnake)

-- Helpers
getFront : Snake -> Vertebra
getFront snake = withDefault defaultVert (head snake)

getBack : Snake -> Vertebra
getBack snake = withDefault defaultVert (head (drop (length snake -1) snake))

getMiddle : Snake -> Vertebra
getMiddle snake = take (length snake - 2) (withDefault [] (tail snake))

getBody : Snake -> Snake
getBody snake = withDefault [] (tail snake)

wrap : Int -> Int
wrap coord =
  if |coord < -gridDimension // 2 -> gridDimension //2
     |coord >= gridDimension // 2 -> -gridDimension // 2
     |otherwise -> coord

noBackwards : Dir -> Vertebra -> Dir
noBackwards arrowDir vert =
  if |arrowDir.x == 0 && vert.dir.x == 0 -> {x = 0, y = 0}
     |arrowDir.y == 0 && vert.dir.y == 0 -> {x = 0, y = 0}
     |otherwise -> arrowDir
  
-- Update

updates : Signal Update
updates =
  let
      delta = map (\t-> t/20) (fps 12)
      isDirectionArrow dir = if dir.x == 0 && dir.y == 0 then False else True
  in
     merge
     (map Tick (every second))
     (map Arrow (filter isDirectionArrow up (sampleOn delta arrows)))

updateState : Update -> State -> State
updateState update state = 
  case update of
    Tick s -> slither state
    Arrow dir -> {state | snake <- turn dir snake}

slither : State -> State
slither state =
  let newVert = scooch (getFront state.snake)
  in
     if (eatsOwnTail newVert snake) then {state | snake <- []} else nibble {state | snake <- [newVert] ++ snake}

turn : Dir -> Snake -> Snake
turn arrowDir snake =
  let correctedDir = noBackwards arrowDir (getFront snake)
  in
      [{getFront snake | dir <- correctedDir}] ++ (getBody snake)

scooch : Vertebra -> Vertebra
scooch front = 
  {
    point = {x =front.point.x + front.dir.x, y = front.point.y + front.dir.y},
    dir = front.dir
  }


eatsOwnTail : Vertebra -> Snake -> Bool

nibble : State -> State


-- View



main = map displaySnake (foldp (\update state -> updateState update state) initState updates)


