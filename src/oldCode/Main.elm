import Html exposing (..)
import AnimationFrame
import Keyboard exposing (..)
import Window exposing (Size)
import Color exposing (Color)
import Collage exposing (..)
import Element exposing (..)
import Text
import String
import Time exposing (Time, second)

--MAIN------------------------------------------------------
--game states
type State
    = Playing
    | Over
    | Pause

--main
main : Program Never
main =
    program
        { init =  defaultGame
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

----GAME---------------------------------------------------

--Game
type alias Game =
    { state : State
    , user : User
    , size : Size
    , score : Float
    }

defaultGame =
      { state = Pause
      , user = initialUser
      , size = Size 0 0
      , score = 0
      , blocks = Block
      }


--KEY---------------------------------------------------
type Msg = KeyMsg Key | Tick Time
type Key = KeyUps Int | KeyDowns Int


subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
    [ Keyboard.downs (KeyDowns >> KeyMsg)
    , Keyboard.ups (KeyUps >> KeyMsg),
    AnimationFrame.diffs Tick
    ]



-- update the model after time and/or keypresses
update msg game =
    case msg of
      Tick time -> tick game
      KeyMsg k -> key k game

--update the model after time and/or keypresses helper
updateWithCommand msg model =
  (update msg model, Cmd.none)
--UPDATE---------------------------------------------------------

runSpead = 4

key msg game =
    game
      |> case msg of
              KeyDowns 37 -> walk -runSpead
              KeyUps 37 -> stop(<)

              KeyDowns 39 -> run runSpead
              KeyUps 39 -> stop(>)

              KeyDowns 32 -> {game | state = Pause}

              KeyDowns 13 -> {game | state = Playing}

              _ ->
                  identity



--USER---------------------------------------------------------
type alias User =
    { x : Float
    , y : Float
    , vx : Float
    , dir : Direction
    }


-- a user initialize
initialUser : User
initialUser =
    { x = 0
    , y = 0
    , vx = 0
    , dir = Right
    }

type Direction
    = Left
    | Right


run : Int -> User -> User
run val user =
    { user
        | vx = toFloat val * 3
        , dir =
            if val < 0 then
                  Left
            else if val > 0 then
                  Right
            else
                  user.dir
    }

updateUser : User -> User
updateUser user =
    user
        |> constraints

constraints : User -> User
constraints user =
    { user
        | vx =
            if krasjVegg user then
                0
            else
                user.vx}

krasjVegg : User -> Bool
krasjVegg user =
    ( withinVegg user leftWall && user.dir == Left )
    || ( withinVegg user rightWall && user.dir Right )

withinVegg : User -> Wall -> Bool
withinVegg user wall =
    near wall.x ((toFloat wall.w) / 2) user.x


near : number -> number -> number -> Bool
near c h n =
    n >= c - h && n <= c + h


--BLOCK-------------------------------------------------------
type alias Wall =
    { h : Int
    , w : Int
    , x : Float
    , y : Float
    }

leftWall : Wall
leftWall =
    Wall 1300 10 -250 0

rightWall : Wall
rightWall =
    Wall 1300 10 250 0

type alias Block =
    { x : Float
    , y : Float
    }

--grid block, grid h=18 (0->17), w=10(0->9)
type alias GridBlock =
    { x : Int
    , y : Int
    }

gridBlock x y = { x = x, y = y }

structure : List (List ( GridBlock ) )
structure =
    [ [ gridBlock 0 0, gridBlock 1 1, gridBlock 2 2, gridBlock 3 3, gridBlock 2 4, gridBlock 1 5, gridBlock 0 6, gridBlock 6 0, gridBlock 7 1, gridBlock 8 2, gridBlock 9 3, gridBlock 8 4, gridBlock 7 5, gridBlock 6 6]
    , [ gridBlock 0 1, gridBlock 1 0, gridBlock 1 1, gridBlock 1 2, gridBlock 2 1, gridBlock 2 5, gridBlock 3 4, gridBlock 3 5, gridBlock 3 6, gridBlock 4 5, gridBlock 4 8, gridBlock 5 7, gridBlock 5 8, gridBlock 5 9, gridBlock 6 8]
    , [ gridBlock 1 0, gridBlock 2 0, gridBlock 3 0, gridBlock 5 2, gridBlock 6 2, gridBlock 7 2, gridBlock 8 2]
    ]


-----VIEW-------------------------------------------------------
--initialize the window
w : number
w = 500
h : number
h = 900

--background Color
backgroundColor =
  Color.black

userColor = Color.darkPurple
userH = 45
userW = 45

view game =
  toHtml (
      collage w h
      [ (filled backgroundColor (rect w h))
      , (moveY -300 (filled (userColor) (rect userW userH )))
      , (moveX leftWall.x (filled ( Color.darkCharcoal (rect leftWall.w leftWall.h) )))
      , (moveX rightWall.x (filled ( Color.darkCharcoal (rect rightWall.w rightWall.h) )))
      ]
  )
