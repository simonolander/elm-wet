module Lane exposing (..)

import Direction


type Lane
    = Left
    | Right


fromDirection : Direction.Direction -> Lane
fromDirection dir =
    case dir of
        Direction.Left -> Left
        Direction.Right -> Right
