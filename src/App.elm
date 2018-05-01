module App exposing (..)

import Collage exposing (Form, collage, filled, group, move, moveX, rect)
import Color exposing (red)
import Geometry
import KeyAction
import Element exposing (Element)
import Html exposing (Html, div, program)
import Html.Attributes exposing (style)
import Keyboard exposing (KeyCode, downs)
import Pointer
import Random
import Task exposing (perform)
import Time exposing (Time, every, millisecond)
import Window exposing (Size, resizes)
import RandomExtra exposing (..)
import AnimationFrame
import Text
import GameController exposing (..)
import Msg exposing (..)
import SquareText


type alias Model =
    { windowSize: Size
    , game: Game
    , frameRate: Time
    , pointerDown: Bool
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
            , gameState = Running
            , numHits = 0
            , character = character
            , remainingLives = 3
            }
        model =
            { windowSize = windowSize
            , game = game
            , frameRate = 0
            , pointerDown = False
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

        PointerDown event ->
            let
                position = event.pointer.clientPos
                x = Tuple.first position
                lane =
                    if x > toFloat model.windowSize.width / 2
                    then
                        Right
                    else
                        Left
            in
                ( { model
                  | game = setCharacterLane lane model.game
                  , pointerDown = True
                  }
                , Cmd.none
                )

        PointerMove event ->
            if model.pointerDown
            then
                let
                    position = event.pointer.clientPos
                    x = Tuple.first position
                    lane =
                        if x > toFloat model.windowSize.width / 2
                        then
                            Right
                        else
                            Left
                in
                    ( { model
                      | game = setCharacterLane lane model.game
                      }
                    , Cmd.none
                    )
            else
                ( model, Cmd.none )

        PointerUp event ->
            ( { model
              | pointerDown = False
              }
            , Cmd.none
            )

        GameGenerated game ->
            ( { model
              | game = game
              }
            , Cmd.none
            )

        GenerateBlock time ->
            ( model, generateBlock model.game)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.game.gameState of
        Running ->
            Sub.batch
                [ resizes Resize
                , AnimationFrame.diffs Tick
                , every (700 * millisecond) GenerateBlock
                , downs KeyDown
                ]

        Paused ->
            Sub.batch
                [ resizes Resize
                , downs KeyDown
                ]


view : Model -> Html Msg
view model =
    let
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
                , ("background", "rgb(18, 43, 23)")
--                , ("background", "linear-gradient( rgba(0, 0, 0, 0.7), rgba(0, 0, 0, 0.7)), url(/assets/forest-puddle.jpg)")
--                , ("background-size", "cover")
--                , ("background-position", "center")
                ]
            , Pointer.onDown PointerDown
            , Pointer.onMove PointerMove
            , Pointer.onUp PointerUp
            ]
            [ div
                [ style
                    [ ("width", toString width ++ "px")
                    , ("height", toString height ++ "px")
                    , ("overflow", "hidden")
--                    , ("background", "#8d94c5")
                    , ("left", left)
                    , ("top", top)
                    , ("position", "absolute")
                    , ("background-image", "url(/assets/forest-puddle.png)")
                    , ("background-repeat", "no-repeat")
                    , ("background-size", "cover")
                    , ("touch-action", "none")
                    ]
                ]
                [ game (width, height) model.game
                , Html.text (toString model.frameRate)
                ]
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
--            , topBar (w, h)
--            , score (width, height) game.score
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
        (form, ww, hh) = SquareText.text (toString score)
        scale = unit * 0.8 * h / hh
        x = w/2 - ww * scale / 2 - w * 0.03
        y = h/2 - hh * scale / 2 - w * 0.03
    in
        form |> Collage.scale scale |> Collage.move (x, y)



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

        sprite : Int -> Form
        sprite index =
            let
                src =
                    if index < remainingLives
                        then "/assets/red-heart.png"
                        else "/assets/grey-heart.png"
            in
                Element.image (round heartWidth) (round heartHeight) src |> Collage.toForm

        move : Int -> Form -> Form
        move index heart =
            Collage.move (-w / 2 + heartWidth * 1.5 * (toFloat index) + heartWidth, h / 2 - heartHeight) heart
    in
        List.range 0 2
        |> List.map sprite
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
                viewCharacter (round w, round h) |> move (x, y)
    in
        group
            [ blocks
            , characterBlock
            ]

viewCharacter : (Int, Int) -> Form
viewCharacter (w, h) =
    Element.image w h "/assets/stickman.png" |> Collage.toForm


block : (Float, Float) -> Block -> Form
block (width, height) block =
    let lane = block.lane
        r = width / 5 / 2
        w = round (r * 2)
        x = case block.lane of
            Left -> -width/2 + r + width / 5
            Right -> -width/2 + r + 3 * width / 5
        y = height * block.y - height / 2 + r
    in
        Element.image w w "/assets/raindrop.png"
        |> Collage.toForm
        |> Collage.move (x, y)


onTick : Time -> Model -> (Model, Cmd Msg)
onTick diff model =
    let
        newModel =
            { model
            | game = updateGame diff model.game
            , frameRate = diff
            }
        cmd = Cmd.none
    in
        ( newModel, cmd )


generateBlock : Game -> Cmd Msg
generateBlock game =
    Random.map2 (addBlockToBoard game) (randomIndex game.boards) randomBlock
    |> Random.generate GameGenerated


circleArc : Float -> Float -> Float -> Float -> Float -> Int -> List (Float, Float)
circleArc cx cy r start rads n =
    let
        point : Int -> (Float, Float)
        point index =
            (cx + r * cos (start + rads * (toFloat index / toFloat n)), cy + r * sin (start + rads * (toFloat index / toFloat n)))
    in
        List.map point (List.range 0 n)
