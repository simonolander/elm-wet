module KeyAction exposing (..)

import Keyboard

type RunningKeyAction
    = MoveLeft
    | MoveRight
    | Pause


type PausedKeyAction
    = Unpause


runningKeyAction : Keyboard.KeyCode -> Maybe RunningKeyAction
runningKeyAction keyCode =
    case keyCode of
        65 -> Just MoveLeft
        68 -> Just MoveRight
        37 -> Just MoveLeft
        39 -> Just MoveRight
        32 -> Just Pause
        _ -> Nothing


pausedKeyAction : Keyboard.KeyCode -> Maybe PausedKeyAction
pausedKeyAction keyCode =
    case keyCode of
        32 -> Just Unpause
        _ -> Nothing
