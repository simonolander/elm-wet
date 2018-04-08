module Direction exposing (..)

import Keyboard


type Direction = Left | Right

direction : Keyboard.KeyCode -> Maybe Direction
direction keycode =
    case keycode of
        65 -> Just Left
        68 -> Just Right
        37 -> Just Left
        39 -> Just Right
        _ -> Nothing
