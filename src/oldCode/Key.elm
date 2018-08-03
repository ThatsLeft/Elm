module Key exposing (..)

type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | NoKey

fromKey : Int -> Key
fromKey keyCode =
    case keyCode of
        37 ->
            ArrowLeft
        39 ->
            ArrowRight
        32 ->
            Space
        _ ->
            NoKey
