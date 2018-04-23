module App exposing (..)

import Collage exposing (Form, collage, filled, group, move, moveX, rect)
import Color exposing (red)
import Geometry
import KeyAction
import Element exposing (Element)
import Html exposing (Html, div, program)
import Html.Attributes exposing (style)
import Keyboard exposing (KeyCode, downs)
import Random
import Task exposing (perform)
import Time exposing (Time, every, millisecond)
import Window exposing (Size, resizes)
import RandomExtra exposing (..)
import AnimationFrame
import Text
import GameController exposing (..)
import Msg exposing (..)


type alias Model =
    { windowSize: Size
    , game: Game
    , time: Time
    }


main : Program Never Model Msg
main = program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


init : (Model, Cmd Msg)
init =
    let windowSize = Size 0 0
        emptyBoard =
            { blocks = []
            , character =
                { lane = Left
                }
            }
        boards =
            [ emptyBoard
            , emptyBoard
            ]
        game =
            { boards = boards
            , score = 0
            , gameState = Paused
            , numHits = 0
            }
        time = 0
        model =
            { windowSize = windowSize
            , game = game
            , time = time
            }
        cmd = Cmd.batch
            [ perform Resize Window.size
            ]
    in
        ( model
        , cmd
        )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Resize size ->
            ( { model | windowSize = size }
            , Cmd.none
            )

        Tick diff -> onTick diff model

        KeyDown keyCode ->
            case model.game.gameState of
                Running ->
                    case KeyAction.runningKeyAction keyCode of
                        Just KeyAction.MoveRight ->
                            ( { model | game = setCharacterLane Right model.game }
                            , Cmd.none
                            )
                        Just KeyAction.MoveLeft ->
                            ( { model | game = setCharacterLane Left model.game }
                            , Cmd.none
                            )
                        Just KeyAction.Pause ->
                            ( { model | game = setGameState Paused model.game }
                            , Cmd.none
                            )
                        Nothing ->
                            ( model
                            , Cmd.none
                            )
                Paused ->
                    case KeyAction.pausedKeyAction keyCode of
                        Just KeyAction.Unpause ->
                            ( { model | game = setGameState Running model.game }
                            , Cmd.none
                            )
                        Nothing ->
                            ( model
                            , Cmd.none
                            )

        GameGenerated game ->
            ( { model
              | game = game
              }
            , Cmd.none
            )

        GenerateBlock time ->
            ( { model | time = time }, generateBlock model.game)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.game.gameState of
        Running ->
            Sub.batch
                [ resizes Resize
                , AnimationFrame.diffs Tick
                , every (300 * millisecond) GenerateBlock
                , downs KeyDown
                ]

        Paused ->
            Sub.batch
                [ resizes Resize
                , downs KeyDown
                ]


view : Model -> Html msg
view model =
    let aspect = 9/16
        windowWidth = model.windowSize.width
        windowHeight = model.windowSize.height
        windowAspect = toFloat windowWidth / toFloat windowHeight
        (width, height) =
            if windowAspect > aspect
            then (round (toFloat windowHeight * aspect), windowHeight)
            else (windowWidth, round (toFloat windowWidth / aspect))
        left =
            if windowAspect > aspect
            then toString ((windowWidth - width) // 2) ++ "px"
            else "0px"
        top =
            if windowAspect > aspect
            then "0px"
            else toString ((windowHeight - height) // 2) ++ "px"
    in
        div
            [ style
                [ ("width", "100%")
                , ("height", "100%")
                , ("overflow", "hidden")
                , ("background", "#94a1fb")
                ]
            ]
            [ div
                [ style
                    [ ("width", toString width ++ "px")
                    , ("height", toString height ++ "px")
                    , ("overflow", "hidden")
                    , ("background", "#8d94c5")
                    , ("left", left)
                    , ("top", top)
                    , ("position", "absolute")
                    , ("background-image", "url(https://img.buzzfeed.com/buzzfeed-static/static/enhanced/webdr03/2013/8/16/16/anigif_enhanced-buzz-25538-1376684244-13.gif)")
                    , ("background-repeat", "no-repeat")
                    , ("background-size", toString width ++ "px" ++ " " ++ toString height ++ "px")
                    ]
                ]
                [ game (width, height) model.game ]
            ]


game : (Int, Int) -> Game -> Html msg
game (width, height) game =
    let boards = game.boards
        w = toFloat width
        h = toFloat height
        bw = w / (toFloat (List.length boards))
    in
        collage width height
            [ boards
                |> List.map (board (bw, h))
                |> List.indexedMap (\ index board -> moveX (-w/2 + bw/2 + bw * (toFloat index)) board)
                |> group
            , score (width, height) game.score
            , numHits (width, height) game.numHits
            ]
            |> Element.toHtml


score : (Int, Int) -> Int -> Form
score (width, height) score =
    let
        w = toFloat width
        h = toFloat height
        scoreString = toString score
        textHeight = 40
        x = 0
        y = h/2 - textHeight/2
    in
        Text.fromString scoreString
        |> Text.height 40
        |> Text.color Color.yellow
        |> Text.monospace
        |> Collage.text
        |> Collage.move (x, y)


numHits : (Int, Int) -> Int -> Form
numHits (width, height) numHits =
    let
        w = toFloat width
        h = toFloat height
        numHitsString = toString numHits
        textHeight = 40
        x = 0
        y = h/2 - textHeight*3/2
    in
        Text.fromString numHitsString
        |> Text.height 40
        |> Text.color Color.red
        |> Text.monospace
        |> Collage.text
        |> Collage.move (x, y)


board : (Float, Float) -> Board -> Form
board (width, height) board =
    let blocks =
            board.blocks
            |> List.map (block (width, height))
            |> group
        character =
            let w = width / 5
                x = case board.character.lane of
                        Left -> -width/2 + w/2 + width / 5
                        Right -> -width/2 + w/2 + 3 * width / 5
                y = w - height / 2
            in
                rect w w |> filled Color.green |> move (x, y)
    in
        group
            [ blocks
            , character
            ]


block : (Float, Float) -> Block -> Form
block (width, height) block =
    let lane = block.lane
        w = width / 5
        x = case block.lane of
            Left -> -width/2 + w/2 + width / 5
            Right -> -width/2 + w/2 + 3 * width / 5
        y = height * block.y - height / 2
    in
        rect w w |> filled Color.orange |> move (x, y)


onTick : Time -> Model -> (Model, Cmd Msg)
onTick diff model =
    let
        newModel =
            { model
            | game = updateGame diff model.game
            }
        cmd = Cmd.none
    in
        ( newModel, cmd )


generateBlock : Game -> Cmd Msg
generateBlock game =
    Random.map2 (addBlockToBoard game) (randomIndex game.boards) randomBlock
    |> Random.generate GameGenerated
