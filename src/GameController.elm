module GameController exposing (..)

import Random
import Time exposing (Time)


type GameState
    = Running
    | Paused

type Lane
    = Left
    | Right

type alias Block =
    { lane: Lane
    , speed: Float
    , y: Float
    , height: Float
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
    , numHits: Int
    }


aspect : Float
aspect = 9/16


updateGame : Time -> Game -> Game
updateGame diff game =
    let updateBlock block =
            moveY (block.speed * diff / 1000) block
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
        |> removePassedBlocks


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
            block.y + block.height > 0

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
        , score = game.score + numRemovedBlocks
        }


randomLane : Random.Generator Lane
randomLane = Random.map (\ leftLane -> if leftLane then Left else Right) Random.bool


randomSpeed : Random.Generator Float
randomSpeed = Random.float -1 -1.1


randomBlock : Random.Generator Block
randomBlock =
    let
        toBlock lane speed =
            { speed = speed
            , y = 1
            , lane = lane
            , height = 1/5 * aspect
            }
    in
        Random.map2 toBlock randomLane randomSpeed


randomIndex : List a -> Random.Generator Int
randomIndex list = Random.int 0 (List.length list - 1)


setGameState : GameState -> Game -> Game
setGameState gameState game =
    { game | gameState = gameState }


moveY : Float -> Block -> Block
moveY dy block =
    { block | y = block.y + dy }
