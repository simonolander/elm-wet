module GameController exposing (..)

import ListUtils exposing (zip)
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
    , y: Float
    , height: Float
    }

type alias Board =
    { blocks: List Block
    }

type alias Game =
    { boards: List Board
    , score: Int
    , gameState: GameState
    , numHits: Int
    , character: Character
    , remainingLives: Int
    }


aspect : Float
aspect = 9/16


unit : Float
unit = 1/5 * aspect * 1/2


updateGame : Time -> Game -> Game
updateGame diff game =
    let
        (boards, offs, hits) =
            game.boards
            |> List.map (updateBoard diff game.character)
            |> ListUtils.unzip3
        score = List.sum offs + game.score
        numHits = List.sum hits + game.numHits
    in
        { game
        | boards = boards
        , score = score
        , numHits = numHits
        , remainingLives = 3 - numHits
        }


updateBoard : Time -> Character -> Board -> (Board, Int, Int)
updateBoard diff character board =
    let
        updateBlock : Block -> Block
        updateBlock block =
            moveY (block.speed * diff / 1000) block

        blockBelowBoard : Block -> Bool
        blockBelowBoard block =
            block.y + block.height < 0

        blockHitsCharacter : Block -> Block -> Bool
        blockHitsCharacter b1 b2 =
            let
                y1 = min b1.y b2.y
                y2 = max (b1.y + b1.height) (b2.y + b2.height)
                c1 = character.y
                c2 = character.y + character.height
            in
                character.lane == b2.lane && (y1 <= c1 && c1 <= y2 || y1 <= c2 && c2 <= y2)

        updateBlocks : List Block -> (List Block, Int, Int)
        updateBlocks blocks =
            case blocks of
                block :: tail ->
                    let
                        updatedBlock = updateBlock block
                        (b, off, hit) = updateBlocks tail
                    in
                        if blockHitsCharacter block updatedBlock
                            then (b, off, hit + 1)
                        else if blockBelowBoard updatedBlock
                            then (b, off + 1, hit)
                        else
                            (updatedBlock :: b, off, hit)
                _ -> ([], 0, 0)

        (newBlocks, off, hit) = updateBlocks board.blocks
    in
        ( { board
          | blocks = newBlocks
          }
        , off
        , hit
        )


setCharacterLane : Lane -> Game -> Game
setCharacterLane lane game =
    let
        character = game.character
        newCharacter =
            { character
            | lane = lane
            }
    in
        { game
        | character = newCharacter
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
            , height = unit
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


merge : (Block, Block) -> Block
merge (b1, b2) =
    let
        minY = min b1.y b2.y
        maxY = max (b1.y + b1.height) (b2.y + b2.height)
        height = maxY - minY
    in
        { b1
        | y = minY
        , height = height
        }
