module Chess exposing (GameState, init)

import Board exposing (Board, File(..), Piece(..), Player(..), Position, Rank(..))


type GameState
    = GameState State


type alias State =
    { board : Board
    , currentTurn : Player
    }


init : GameState
init =
    GameState
        { board = Board.init (append whiteInit blackInit)
        , currentTurn = White
        }


whiteInit : List ( Piece, Position )
whiteInit =
    [ ( Rook White, ( A, One ) )
    , ( Knight White, ( B, One ) )
    , ( Bishop White, ( C, One ) )
    , ( Queen White, ( D, One ) )
    , ( King White, ( E, One ) )
    , ( Bishop White, ( F, One ) )
    , ( Knight White, ( G, One ) )
    , ( Rook White, ( H, One ) )
    , ( Pawn White, ( A, Two ) )
    , ( Pawn White, ( B, Two ) )
    , ( Pawn White, ( C, Two ) )
    , ( Pawn White, ( D, Two ) )
    , ( Pawn White, ( E, Two ) )
    , ( Pawn White, ( F, Two ) )
    , ( Pawn White, ( G, Two ) )
    , ( Pawn White, ( H, Two ) )
    ]


blackInit : List ( Piece, Position )
blackInit =
    [ ( Rook Black, ( A, Eight ) )
    , ( Knight Black, ( B, Eight ) )
    , ( Bishop Black, ( C, Eight ) )
    , ( Queen Black, ( D, Eight ) )
    , ( King Black, ( E, Eight ) )
    , ( Bishop Black, ( F, Eight ) )
    , ( Knight Black, ( G, Eight ) )
    , ( Rook Black, ( H, Eight ) )
    , ( Pawn Black, ( A, Seven ) )
    , ( Pawn Black, ( B, Seven ) )
    , ( Pawn Black, ( C, Seven ) )
    , ( Pawn Black, ( D, Seven ) )
    , ( Pawn Black, ( E, Seven ) )
    , ( Pawn Black, ( F, Seven ) )
    , ( Pawn Black, ( G, Seven ) )
    , ( Pawn Black, ( H, Seven ) )
    ]
