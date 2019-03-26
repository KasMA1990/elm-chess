module Chess exposing (GameState, init)

import Board exposing (Board, File(..), Move, MoveError(..), Piece(..), Player(..), Position, Rank(..), blackInit, whiteInit)


type GameState
    = GameState State


type alias State =
    { board : Board
    , currentTurn : Player
    , potentialMoves : List ( Piece, List Move )
    }


init : GameState
init =
    let
        currentPlayer =
            White

        board =
            Board.init (append whiteInit blackInit)
    in
    GameState
        { board = board
        , currentTurn = currentPlayer
        , potentialMoves = Board.getValidMoves currentPlayer board
        }


makeMove : GameState -> Move -> GameState
makeMove fullState move =
    let
        (GameState state) =
            fullState

        nextPlayer =
            case state.currentTurn of
                White ->
                    Black

                Black ->
                    White
    in
    case Board.executeMove state.board move of
        Ok newBoard ->
            GameState
                { board = newBoard
                , currentTurn = nextPlayer
                , potentialMoves = Board.getValidMoves nextPlayer newBoard
                }

        -- TODO What would nice error handling look like in this case?
        Err StaleMove ->
            fullState
