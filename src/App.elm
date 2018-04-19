module App exposing (..)

import Collage exposing (Form, collage, filled, group, move, moveX, rect)
import Color exposing (red)
import KeyAction
import Element exposing (Element)
import Html exposing (Html, div, program)
import Html.Attributes exposing (style)
import Keyboard exposing (KeyCode, downs)
import Lane exposing (Lane)
import Random
import Task exposing (perform)
import Time exposing (Time, every, millisecond)
import Window exposing (Size, resizes)
import RandomExtra exposing (..)
import AnimationFrame
import Text


type alias Model =
    { windowSize: Size
    , game: Game
    , time: Time
    }

type GameState
    = Running
    | Paused

type alias Block =
    { lane: Lane
    , height: Float
    , speed: Float
    }

type alias Character =
    { lane: Lane
    }

type alias Board =
    { blocks: List Block
    , character: Character
    }

type alias Game =
    { boards: List Board
    , score: Int
    , gameState: GameState
    }

type Msg
    = Tick Time
    | Resize Size
    | KeyDown KeyCode
    | GenerateBlock Time
    | GameGenerated Game


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
                { lane = Lane.Left
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
                            ( { model | game = setCharacterLane Lane.Right model.game }
                            , Cmd.none
                            )
                        Just KeyAction.MoveLeft ->
                            ( { model | game = setCharacterLane Lane.Left model.game }
                            , Cmd.none
                            )
                        Just KeyAction.Pause ->
                            ( setGameState Paused model
                            , Cmd.none
                            )
                        Nothing ->
                            ( model
                            , Cmd.none
                            )
                Paused ->
                    case KeyAction.pausedKeyAction keyCode of
                        Just KeyAction.Unpause ->
                            ( setGameState Running model
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


board : (Float, Float) -> Board -> Form
board (width, height) board =
    let blocks =
            board.blocks
            |> List.map (block (width, height))
            |> group
        character =
            let w = width / 5
                x = case board.character.lane of
                        Lane.Left -> -width/2 + w/2 + width / 5
                        Lane.Right -> -width/2 + w/2 + 3 * width / 5
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
            Lane.Left -> -width/2 + w/2 + width / 5
            Lane.Right -> -width/2 + w/2 + 3 * width / 5
        y = height * block.height - height / 2
    in
        rect w w |> filled Color.orange |> move (x, y)


onTick : Time -> Model -> (Model, Cmd Msg)
onTick diff model =
    let
        newModel =
            { model
            | game = updateGame diff model.game |> removePassedBlocks
            }
        cmd = Cmd.none
    in
        ( newModel, cmd )


updateGame : Time -> Game -> Game
updateGame diff game =
    let updateBlock block =
            { block
            | height = block.height - block.speed * diff / 1000
            }
        updateBoard board =
            { board
            | blocks = List.map updateBlock board.blocks
            }
        updatedGame =
            { game
            | boards = List.map updateBoard game.boards
            }
    in
        updatedGame


randomLane : Random.Generator Lane
randomLane = Random.map (\ leftLane -> if leftLane then Lane.Left else Lane.Right) Random.bool


randomSpeed : Random.Generator Float
randomSpeed = Random.float 1 1.1


randomBlock : Random.Generator Block
randomBlock =
    let toBlock lane speed = { lane = lane, speed = speed, height = 1.1 }
    in
        Random.map2 toBlock randomLane randomSpeed


generateBlock : Game -> Cmd Msg
generateBlock game =
    Random.map2 (addBlockToBoard game) (randomIndex game.boards) randomBlock
    |> Random.generate GameGenerated


setCharacterLane : Lane -> Game -> Game
setCharacterLane lane game =
    let
        character : Lane -> Character -> Character
        character lane character =
            { character | lane = lane }
        board : Lane -> Board -> Board
        board lane board =
            { board | character = character lane board.character}
    in
        { game
        | boards = List.map (board lane) game.boards
        }


randomIndex : List a -> Random.Generator Int
randomIndex list = Random.int 0 (List.length list - 1)


addBlockToBoard : Game -> Int -> Block -> Game
addBlockToBoard game boardIndex block =
    let addBlockIfCorrectIndex : Int -> Board -> Board
        addBlockIfCorrectIndex index board =
            if index == boardIndex
                then { board
                     | blocks = block :: board.blocks
                     }
                else board
    in
        { game
        | boards =
            List.indexedMap addBlockIfCorrectIndex game.boards
        }


removePassedBlocks : Game -> Game
removePassedBlocks game =
    let filterBlock : Block -> Bool
        filterBlock block =
            block.height > -1

        filterBoard : Board -> Board
        filterBoard board =
            { board
            | blocks = List.filter filterBlock board.blocks
            }

        newBoards : List Board
        newBoards =
            List.map filterBoard game.boards

        countBlocks : List Board -> Int
        countBlocks boards =
            boards
            |> List.map (List.length << .blocks)
            |> List.sum

        numRemovedBlocks : Int
        numRemovedBlocks = countBlocks game.boards - countBlocks newBoards

    in
        { game
        | boards = newBoards
        , score = game.score + numRemovedBlocks * 10
        }


setGameState : GameState -> Model -> Model
setGameState gameState model =
    let game = model.game
    in
        { model | game = { game | gameState = gameState } }
