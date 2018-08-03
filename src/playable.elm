-- Elm dodge the blocks game
-- Thomas Mannsverk Eliassen && Kristian Nilssen

import Html exposing (program, Html)
import AnimationFrame
import Keyboard exposing (..)
import Array exposing (..)
import Random exposing (..)
import Color exposing (Color)
import Collage exposing (..)
import Element exposing (..)
import Text exposing (..)
import Time exposing (Time, second)

--MAIN------------------------------------------------------
--game states
type State
    = Starting
    | Playing
    | Over
    | Pause

--main

main = program
        { init =  (defaultGame, generateInitialSeed)
        , view = view
        , update = updateWithCommand
        , subscriptions = subscriptions
        }

generateInitialSeed = Random.generate InitialSeed (Random.int Random.minInt Random.maxInt )

--KEY---------------------------------------------------
type Msg = KeyMsg Key | Tick Time | InitialSeed Int
type Key = KeyUps Int | KeyDowns Int

--Game
type alias Game =
    { player : User
    , score : Float
    , state : State
    , seed : Random.Seed
    , boks : List (Block)
    , tickTime : Int
    , collision : List (Block)
    }

defaultGame =
    { player = initialUser
    , score = 0.0
    , state = Starting
    , seed = Random.initialSeed 0
    , boks = []
    , tickTime = 0
    , collision = []
    }

type alias User =
    { x : Float
    , vx : Float
    , y : Float
    , playerAcc : Float
    }

initialUser =
    { x = 0
    , vx = 0
    , y = -270
    , playerAcc = 0
    }


subscriptions game =
    Sub.batch
    [ Keyboard.downs (KeyDowns >> KeyMsg)
    , Keyboard.ups (KeyUps >> KeyMsg)
    , AnimationFrame.diffs Tick
    ]

updateWithCommand msg game =
  (update msg game, Cmd.none)

update : Msg -> Game -> Game
update msg game =
    case msg of
      InitialSeed val -> ({game | seed = Random.initialSeed val })
          |> addNewItem
      Tick time -> tick game
      KeyMsg k -> key k game

------Blocks and structures----------------------------
type alias Block =
    { x : Float
    , y : Float
    }

--grid block, grid h= -14->14, w=-10->10
type alias GridBlock =
    { x : Int
    , y : Int
    }

gridBlock x y = { x = x, y = y }

blockMake x y = { x = x, y = y }
--structure : List List (GridBlock))
structure =
    Array.fromList [
      [ gridBlock -9 13, gridBlock -7 11, gridBlock -5 9, gridBlock -3 7, gridBlock -1 5, gridBlock 1 3, gridBlock -1 1, gridBlock -3 -1, gridBlock -5 -3, gridBlock -7 -5, gridBlock -9 -7,
        gridBlock 3 13, gridBlock 5 11, gridBlock 7 9, gridBlock 9 7, gridBlock 9 -1, gridBlock 7 -3, gridBlock 5 -5, gridBlock 3 -7 ]
    , [ gridBlock -1 13, gridBlock 1 13,
        gridBlock -1 11, gridBlock 1 11,
        gridBlock -5 9, gridBlock -3 9, gridBlock -1 9, gridBlock 1 9, gridBlock 3 9, gridBlock 5 9,
        gridBlock -5 7, gridBlock -3 7, gridBlock -1 7, gridBlock 1 7, gridBlock 3 7, gridBlock 5 7,
        gridBlock -1 5, gridBlock 1 5,
        gridBlock -1 3, gridBlock 1 3 ]
    , [ gridBlock -9 -1, gridBlock -7 -1, gridBlock -5 -1, gridBlock -3 -1, gridBlock -1 -1, gridBlock 1 -1,
        gridBlock -3 13, gridBlock -1 11, gridBlock 1 9, gridBlock 3 7, gridBlock 5 9, gridBlock 7 11, gridBlock 9 13 ]
    , [ gridBlock 0 3, gridBlock 0 5, gridBlock -9 5, gridBlock 9 5, gridBlock -2 7, gridBlock 2 7, gridBlock 4 9, gridBlock -4 9, gridBlock -5 11, gridBlock 5 11, gridBlock -5 13, gridBlock 5 13 ]
    ]

accesBlockinList : List (GridBlock) -> List (Block)
accesBlockinList l =
    List.map multiplBlock l

multiplBlock : GridBlock -> Block
multiplBlock g =
    blockMake (toFloat (g.x*25)) (toFloat (g.y*25+550))


getChoice : Int -> List (Block)
getChoice i =
    accesBlockinList (Maybe.withDefault [] ( (Array.get i structure) ) )


addNewItem game =
        let (randomVal, newSeed ) = Random.step (Random.int 0 (Array.length structure) ) game.seed in
                { game | seed =  newSeed
                       , boks = List.append (getChoice randomVal)  game.boks
                        }



--update the model after time and/or keypresses helper
--UPDATE---------------------------------------------------------
blockFall = -8.0
runSpeed = 9

key msg game =
  case game.state of
    Playing ->
    game
      |> case msg of
              --A
              KeyDowns 65 -> run (0-runSpeed)
              KeyUps 65 -> stop (<)
              --D
              KeyDowns 68 -> run runSpeed
              KeyUps 68 -> stop (>)
              --Space for pause
              KeyDowns 32 -> fucPause
              _ ->
                  identity

    Starting ->
      game
       |> case msg of
             KeyDowns 13 -> unfucPause
             _ ->
                 identity
    Over ->
      game
        |> case msg of
              KeyDowns 13 -> startOver
              _ ->
                  identity
    Pause ->
      game
     |> case msg of
           KeyDowns 13 -> unfucPause
           _ ->
               identity

startOver game =
   defaultGame
fucPause game =
    { game | state = Pause}
unfucPause game =
    { game | state = Playing}

run speed game =
    let p = game.player in
      {game | player = {p | playerAcc = speed }}

stop condition game =
    if condition game.player.playerAcc 0 then
      let p = game.player in
        {game | player = {p | playerAcc = 0}}
    else
      game

tick : Game -> Game
tick game =
    game
        |> kill
        |> stateCheck
        |> fallingBloks
        |> walls
        |> moving
        |> removeStruc
        |> tickTack
        |> addnewBlock

--tickTack
tickTack : Game -> Game
tickTack game =
  if game.state == Playing then
    let t = game.tickTime in
      { game | tickTime = t + 1}
  else
    game

--remove the structures
removeStruc : Game -> Game
removeStruc game =
    let b = game.boks in
      { game | boks = List.filter checkY b}

checkY : Block -> Bool
checkY bl =
    if bl.y < -400 then
      False
    else
      True

--kill user------------------------------------------
kill : Game -> Game
kill game =
  if List.any (testHit game) game.boks == True then
    { game | state = Over
           , collision = List.filter (testHit game) game.boks}
  else
    game


testHit : Game -> Block -> Bool
testHit game b =
    if abs (b.y - game.player.y) <= 47.5 && abs (b.x - game.player.x) <= 47.5 then
        True
    else
        False

---------------------------------------------------------
addnewBlock : Game -> Game
addnewBlock game =
    if game.tickTime >= 100 && game.state == Playing then
        addNewItem { game | tickTime = 0}
    else
        game

-- kill game =
moving : Game -> Game
moving game =
    let p =  game.player in
      { game | player = {p | x = game.player.x + game.player.playerAcc } }

walls : Game -> Game
walls game =
    if game.player.x+17 >= rightWall.x-10 then
      let p = game.player in
        {game | player = { p | x = rightWall.x - 23}}
    else if game.player.x-17 <= leftWall.x+10 then
      let p = game.player in
        {game | player = { p | x = leftWall.x + 23}}
    else
      game

--move blocks down
fallingBloks : Game -> Game
fallingBloks game =
    if game.state == Playing then
      let b = game.boks in
        { game | boks = List.map applySpeed b }
    else
      game


applySpeed : Block -> Block
applySpeed s =
    { s | y = s.y+blockFall }

--check state----------------------------------------------------------------
stateCheck : Game -> Game
stateCheck game =
    if game.state == Pause then
      let p = game.player in
        { game | player = {p | playerAcc = 0 }}
    else if game.state == Playing then
      let s = game.score in
        { game | score = game.score + 0.05}
    -- else if game.state == Over then
      -- how to go tho the death/over state
    else if game.state == Over then
      let b = game.boks in
        { game | boks = [] }
    else
        game

--walls
type alias Wall =
    { h : Float
    , w : Float
    , x : Float
    , y : Float
    }

leftWall : Wall
leftWall =
    Wall 1300 10 -250 0

rightWall : Wall
rightWall =
    Wall 1300 10 250 0

bottwall : Wall
bottwall =
    Wall 10 500 0 -350

w : number
w = 500
h : number
h = 700

--background Color
backgroundColor =
  Color.darkCharcoal

pauseMelding =
  "Game Paused \n\n Unpause by hitting \"Enter\" \n\n Current score is: "
gameName =
  "Avoid the blocks"
startGame =
  "Welcome, to start the game hit \"Enter\" \n\n Use \"A\" and \"D\" to controll the block \n\n and use \"Space\" so pause the game"
deadMelding =
    "You died \n\n to try again hit \"Enter\" \n\n Score: "

blockColor =  Color.darkRed
blockH = 50
blockW = 50

userColor = Color.darkPurple
userH = 45
userW = 45

view : Game -> Html Msg
view game =
  if game.state == Pause then
    toHtml (
        collage w h
        [ (filled backgroundColor (rect w h))
        , (move (0, 200) (toForm (centered (fromString (gameName)))))
        , pauseMelding ++ toString (truncate game.score)
              |> fromString
              |> centered
              |> toForm
        , (moveY game.player.y (moveX (game.player.x)(filled (userColor) (rect userW userH ))))
        , group (List.map renderBoks game.boks)
        , (moveX leftWall.x (filled ( Color.darkRed ) (rect leftWall.w leftWall.h) ))
        , (moveX rightWall.x (filled ( Color.darkRed ) (rect rightWall.w rightWall.h) ))
        , (moveY bottwall.y (filled ( Color.darkRed) (rect bottwall.w bottwall.h) ))
        ]
    )
  else if game.state == Starting then
    toHtml (
        collage w h
        [ (filled backgroundColor (rect w h))
        , (move (0, 200) (toForm (centered (fromString (gameName)))))
        , (toForm (centered (fromString (startGame))))
        , (moveY game.player.y (moveX (game.player.x)(filled (userColor) (rect userW userH ))))
        , group (List.map renderBoks game.boks)
        , (moveX leftWall.x (filled ( Color.darkRed) (rect leftWall.w leftWall.h) ))
        , (moveX rightWall.x (filled ( Color.darkRed) (rect rightWall.w rightWall.h) ))
        , (moveY bottwall.y (filled ( Color.darkRed) (rect bottwall.w bottwall.h) ))
        ]
    )
  else if game.state == Playing then
    toHtml (
        collage w h
        [ (filled backgroundColor (rect w h))
        , (move (0, 200) (toForm (centered (fromString (gameName)))))
        , "Score: " ++ toString (truncate game.score)
              |> fromString
              |> centered
              |> toForm
              |> move (-210,340)
        , (moveY game.player.y (moveX (game.player.x)(filled (userColor) (rect userW userH ))))
        , group (List.map renderBoks game.boks)
        , (moveX leftWall.x (filled ( Color.darkRed) (rect leftWall.w leftWall.h) ))
        , (moveX rightWall.x (filled ( Color.darkRed) (rect rightWall.w rightWall.h) ))
        , (moveY bottwall.y (filled ( Color.darkRed) (rect bottwall.w bottwall.h) ))
        ]
    )
  -- dead
  else
    toHtml (
      collage w h
      [ (filled backgroundColor (rect w h))
      , (move (0, 200) (toForm (centered (fromString (gameName)))))
      , deadMelding ++ toString (truncate game.score)
              |> fromString
              |> centered
              |> toForm
      --, (moveY game.player.y (moveX (game.player.x)(filled (userColor) (rect userW userH ))))
      , (moveX leftWall.x (filled ( Color.darkRed) (rect leftWall.w leftWall.h) ))
      , (moveX rightWall.x (filled ( Color.darkRed) (rect rightWall.w rightWall.h) ))
      , (moveY bottwall.y (filled ( Color.darkRed) (rect bottwall.w bottwall.h) ))
      ]
    )


renderBoks : Block -> Form
renderBoks b =
    (moveX b.x (moveY b.y (filled (blockColor) (rect blockW blockH))))
