import Random exposing (..)


type alias Block =
    { x : Float
    , y : Float
    }

--grid block, grid h=18 (0->17), w=10(0->9)
type alias GridBlock =
    { x : Int
    , y : Int
    }

blockMake x y = { x = x, y = y }

gridBlock x y = { x = x, y = y }

--structure : List List (GridBlock))
structure =
    [ [ gridBlock -9 13, gridBlock -7 11, gridBlock -5 9, gridBlock -3 7, gridBlock -5 5, gridBlock -7 3, gridBlock -9 1, gridBlock 3 13, gridBlock 5 11, gridBlock 7 9, gridBlock 9 7, gridBlock 7 5, gridBlock 5 3, gridBlock 3 1]
    , [ gridBlock -7 13, gridBlock -9 11, gridBlock -7 11, gridBlock -5 11, gridBlock -7 9, gridBlock 1 9, gridBlock -1 7, gridBlock 1 7, gridBlock 3 7, gridBlock 1 5, gridBlock 7 5, gridBlock 5 3, gridBlock 7 3, gridBlock 9 3, gridBlock 7 1]
    , [ gridBlock -7 13, gridBlock -5 13, gridBlock -3 13, gridBlock 1 9, gridBlock 3 9, gridBlock 5 9, gridBlock 7 9]
    , [ gridBlock -5 13, gridBlock -3 11, gridBlock -1 9, gridBlock 1 7, gridBlock 3 9, gridBlock 5 11, gridBlock 7 13]
    ]


returnListFromListList s =
  map accesBlockinList s

accesBlockinList l =
    map multiplBlock l

multiplBlock g =
    blockMake (g.x*25) (g.y*25)
