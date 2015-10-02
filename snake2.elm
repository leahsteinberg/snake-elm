import Keyboard exposing (arrows)
import Window
import Signal exposing (map, foldp, merge, filter, sampleOn)
import Time exposing (every, second, Time, fps)
import List exposing (head, tail, take, drop, length, foldr)
import Maybe exposing (withDefault)
import Graphics.Element exposing (image, container, middle, Element)
import Graphics.Collage exposing (collage, rect, ngon, filled, move, rotate, toForm, Form)
import Color exposing (lightRed, white, darkRed, Color)
import Random exposing (int, generate, Seed, Generator, initialSeed)


-- Model

type alias Snake = List (Vertebra)
type alias Vertebra = {point: Point, dir: Dir}
type alias Point = {x: Int, y: Int}
type alias Dir = {x: Int, y: Int}
type alias Food = Point
type alias State = (Food, Snake, Seed)
type Update = Tick Time | Arrow Dir

pointDim = 10
pointOffset = 15
gridDim = 44
defaultVert = {point= {x=0, y=0}, dir= {x=1, y =0}}
upArrow = {x=0, y =1}
initFood = {x= 5, y = 5}
newFood = {x= -5, y = -5}

initSnake = [
  {point= {x= -1, y=0}, dir= {x= -1, y= 0}}
  ,{point= {x= 0, y=0}, dir= {x= -1, y= 0}}
  ,{point= {x= 1, y=0}, dir= {x= -1, y= 0}}
  ,{point= {x= 1, y=1}, dir= {x= 0, y= -1}}
  ,{point= {x= 1, y=2}, dir= {x= 0, y= -1}}
  ,{point= {x= 1, y=3}, dir= {x= 0, y= -1}}
  ,{point= {x= 2, y=3}, dir= {x= -1, y= 0}}]


initState = (initFood, initSnake, initialSeed 5)

-- Helpers
getFront : Snake -> Vertebra
getFront snake = withDefault defaultVert (head snake)

getBack : Snake -> Vertebra
getBack snake = withDefault defaultVert (head (drop ((length snake) - 1) snake))

getMiddle : Snake -> Snake
getMiddle snake = take (length snake - 2) (withDefault [] (tail snake))

getBody : Snake -> Snake
getBody snake = withDefault [] (tail snake)

wrap : Int -> Int
wrap coord =
  let maxDim = gridDim // 2
  in
      if |coord < -gridDim // 2 -> gridDim // 2
         |coord >= gridDim //2 -> -gridDim //2
         |otherwise -> coord

noBackwards : Dir -> Vertebra -> Dir
noBackwards arrowDir vert =
  if |arrowDir.x == 0 && vert.dir.x == 0 -> vert.dir
     |arrowDir.y == 0 && vert.dir.y == 0 -> vert.dir
     |otherwise -> arrowDir

--newFood : Food

-- Update

updates : Signal Update
updates =
  let
      delta = map (\t-> t/20) (fps 12)
      isDirectionArrow dir = if dir.x == 0 && dir.y == 0 then False else True
  in
     merge
     (map Tick (fps 5))
     (map Arrow (filter isDirectionArrow upArrow (sampleOn delta arrows)))

updateState : Update -> State -> State
updateState update state =
  let (food, snake, seed) = state
  in
      case update of
        Tick s -> slither state
        Arrow dir -> (food, turn dir snake, seed)

slither : State -> State
slither state =
  let (food, snake,seed) = state
      newVert = scooch (getFront snake)
      snakeMiddle = getMiddle snake
  in
     if (pointOnSnake newVert.point snake) then (food, [], seed) else nibble (food, [newVert] ++ [getFront snake] ++ snakeMiddle, seed)

turn : Dir -> Snake -> Snake
turn arrowDir snake =
  let correctedDir = noBackwards arrowDir (getFront snake)
      front = getFront snake
  in
      [{front | dir <- correctedDir}] ++ (getBody snake)



addTail : Snake -> Snake
addTail snake =
  let back = getBack snake
      newBack = {back | point <- {x = back.point.x - back.dir.x,
                y = back.point.y - back.dir.y}}
  in
     snake ++ [newBack]


scooch : Vertebra -> Vertebra
scooch front =
  {
    point = {x = wrap (front.point.x + front.dir.x), y = wrap(front.point.y + front.dir.y)},
    dir = front.dir
  }


chomp : {a | x: Int, y: Int} -> {a | x: Int, y: Int} -> Bool
chomp fangs morsel = fangs.x == morsel.x && fangs.y == morsel.y

pointOnSnake : Point -> Snake -> Bool
pointOnSnake newPoint snake =
  let oldSnake = [getFront snake] ++ (getMiddle snake)
  in
     foldr (\vert accum -> if chomp newPoint vert.point then True else accum) False oldSnake

digest : State -> State
digest state =
  let (food, snake, seed) = state
      (newInt, newSeed) = generate (int -13 13) seed
      (newerInt, newerSeed) = generate (int -13 13) newSeed
      newFood = {x = newInt, y = newerInt}
  in
     if not (pointOnSnake newFood snake) then (newFood, addTail snake, newerSeed) else digest (food, snake, newerSeed)


nibble : State -> State
nibble state =
  let (food, snake, seed) = state
      frontVert = getFront snake
  in
     if chomp food frontVert.point then digest state else state
-- View

drawPoint : Color -> {a | x: Int, y: Int} -> Form
drawPoint color point = rect pointDim pointDim
                          |> filled color
                          |> move (toFloat point.x * pointOffset, toFloat point.y * pointOffset)

displayState : State -> Element
displayState state =
  let (food, snake, seed) = state
      pixelDim = gridDim * pointOffset
  in
     collage pixelDim pixelDim ([drawPoint darkRed food] ++ (List.map (\vert -> (drawPoint lightRed) vert.point) snake))

main = map displayState (foldp (\update state -> updateState update state) initState updates)


