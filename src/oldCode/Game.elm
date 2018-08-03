module Game exposing (..)

import Block exposing (..)
import Window exposing (Size)
import Task
import Random exposing (Generator)
import Keyboard exposing (KeyCode)
import Key exposing (..)
import Time exposing (Time, second)

----GAME---------------------------------------------------
--game states
type State
    = Playing
    | Over
    | Pause

--Game
type alias Game =
    { state : State
    , user : User
    , structures : List ( List ( GridBlock ))
    , size : Size
    , score : Float
    }


--Msg
type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | NewStructure
    | Rezise Size
    | NoOp


defaultGame = ( Game, Cmd Msg )
defaultGame =
    ( { state = Pause
      , user = initialUser
      , structures =  None
      , size = Size 0 0
      , score = 0
      }
    , Task.perform (\_ -> NoOp) Resize (Window.size)
    )
