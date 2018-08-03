module User exposing (..)

import Block exposing (..)

type alias User =
    { x : Float
    , y : Float
    , vx : Float
    }


-- a user initialize
initialUser : User
initialUser =
    { x = 0
    , y = 0
    , vx = 0
    }

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
