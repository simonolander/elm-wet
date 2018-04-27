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
            }
        boards =
            [ emptyBoard
            , emptyBoard
            ]
        character =
            { lane = Left
              , y = 0.05
              , height = unit
            }
        game =
            { boards = boards
            , score = 0
            , gameState = Paused
            , numHits = 0
            , character = character
            , remainingLives = 3
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
--                , ("background", "rgb(18, 43, 23)")
                , ("background", "linear-gradient( rgba(0, 0, 0, 0.7), rgba(0, 0, 0, 0.7)), url(/assets/forest-puddle.jpg)")
--                , ("background-repeat", "no-repeat")
                , ("background-size", "cover")
                , ("background-position", "center")

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
                    , ("background-image", "url(/assets/forest-puddle-animation.gif)")
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
                |> List.map (board (bw, h) game.character)
                |> List.indexedMap (\ index board -> moveX (-w/2 + bw/2 + bw * (toFloat index)) board)
                |> group
            , topBar (w, h)
            , score (width, height) game.score
            , lives (width, height) game.remainingLives
            ]
            |> Element.toHtml


topBar : (Float, Float) -> Form
topBar (w, h) =
    let
        height = h * unit * 1.4
        grad = Color.linear
            ( 0, height / 2 )
            ( 0, -height / 2 )
            [ ( 0, Color.black )
            , ( 1, Color.rgba 0 0 0 0)
            ]
    in
        rect w height |> Collage.gradient grad |> Collage.moveY (h / 2 - height / 2)


score : (Int, Int) -> Int -> Form
score (width, height) score =
    let
        w = toFloat width
        h = toFloat height
        scoreString = toString score
        textHeight = unit * 0.9 * h
        x = 0
        y = h/2 - textHeight/2
    in
        Text.fromString scoreString
        |> Text.height textHeight
        |> Text.color Color.yellow
        |> Text.monospace
        |> Collage.text
        |> Collage.move (x, y)


lives : (Int, Int) -> Int -> Form
lives (width, height) remainingLives =
    let
        w = toFloat width
        h = toFloat height

        heartWidth = unit * 2/3 * h
        heartHeight = unit * 2/3 * h

        color : Int -> Collage.Shape -> Form
        color index heart =
            filled (if index < remainingLives then Color.red else Color.darkCharcoal) heart

        move : Int -> Form -> Form
        move index heart =
            Collage.move (-w / 2 + heartWidth * 1.5 * (toFloat index) + heartWidth, h / 2 - heartHeight) heart
    in
        List.repeat 3 (heart heartWidth heartHeight)
        |> List.indexedMap color
        |> List.indexedMap move
        |> group


heart : Float -> Float -> Collage.Shape
heart width height =
    let
        -- r * 2 + sqrt(2*r**2) = width
        r = width * (1 - sqrt(2) / 2)
        h = 4 * r / sqrt 2
        cx = width / 2 - r
        cy = h / 2 - r
        halfCircle cx cy angle =
            let
                n = 10
                point : Int -> (Float, Float)
                point index =
                    (cx + r * cos (angle + pi * (toFloat index / toFloat n)), cy + r * sin (angle + pi * (toFloat index / toFloat n)))
            in
                List.map point (List.range 0 n)
        rightHalf = halfCircle cx cy (-pi / 4)
        leftHalf = halfCircle -cx cy (pi / 4)
    in
        Collage.polygon
            ((0, -h/2) :: rightHalf ++ leftHalf)


board : (Float, Float) -> Character -> Board -> Form
board (width, height) character board =
    let blocks =
            board.blocks
            |> List.map (block (width, height))
            |> group
        characterBlock =
            let
                w = width / 5
                h = character.height * height
                x = case character.lane of
                        Left -> -width/2 + w/2 + width / 5
                        Right -> -width/2 + w/2 + 3 * width / 5
                y = height * character.y + w / 2 - height / 2
            in
                rect w h |> filled Color.green |> move (x, y)
    in
        group
            [ blocks
            , characterBlock
            ]


block : (Float, Float) -> Block -> Form
block (width, height) block =
    let lane = block.lane
        w = width / 5
        x = case block.lane of
            Left -> -width/2 + w/2 + width / 5
            Right -> -width/2 + w/2 + 3 * width / 5
        y = height * block.y - height / 2 + w / 2
    in
        rect w w |> filled (Color.rgb 43 128 255) |> move (x, y)


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
