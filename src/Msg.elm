module Msg exposing (..)

import GameController exposing (Game)
import Keyboard exposing (KeyCode)
import Time exposing (Time)
import Window exposing (Size)


type Msg
    = Tick Time
    | Resize Size
    | KeyDown KeyCode
    | GenerateBlock Time
    | GameGenerated Game
