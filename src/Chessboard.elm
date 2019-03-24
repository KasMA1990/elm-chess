module Chessboard exposing (Board, newBoard)

import Array exposing (Array, set)
import Html exposing (Html, table)
import Main exposing (Msg)


type Board
    = Board FullBoard


type alias FullBoard =
    { squares : Squares
    , currentTurn : Player
    }


type alias Squares =
    Array SquareState


newBoard : Board
newBoard =
    let
        initPositions =
            append whiteInit blackInit

        emptySquares =
            Array.repeat 64 Free

        squares =
            initSquares initPositions emptySquares
    in
    Board
        { squares = squares
        , currentTurn = White
        }


initSquares : List ( Piece, Position ) -> Squares -> Squares
initSquares pieces squares =
    case pieces of
        ( piece, pos ) :: [] ->
            set (getIndex pos) (Taken piece) squares

        ( piece, pos ) :: tail ->
            set (getIndex pos) (Taken piece) (initSquares tail)


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


viewBoard : Board -> Html Msg
viewBoard board =
    case board of
        Board fullBoard ->
            table [] []


type SquareState
    = Free
    | Taken Piece


type Piece
    = King Player
    | Queen Player
    | Rook Player
    | Bishop Player
    | Knight Player
    | Pawn Player


type Rank
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight


type File
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


type alias Position =
    ( File, Rank )


type Player
    = Black
    | White


getIndex : Position -> Int
getIndex ( file, rank ) =
    getFileNumeral file * getFileNumeral H + getRankNumeral rank


getPosition : Int -> Position
getPosition index =
    let
        maxFileNumeral =
            getFileNumeral H

        rank =
            modBy maxFileNumeral index

        file =
            round (toFloat (index - rank) / toFloat maxFileNumeral)
    in
    ( getFile file, getRank rank )


getRankNumeral : Rank -> Int
getRankNumeral rank =
    case rank of
        One ->
            0

        Two ->
            1

        Three ->
            2

        Four ->
            3

        Five ->
            4

        Six ->
            5

        Seven ->
            6

        Eight ->
            7


getFileNumeral : File -> Int
getFileNumeral file =
    case file of
        A ->
            0

        B ->
            1

        C ->
            2

        D ->
            3

        E ->
            4

        F ->
            5

        G ->
            6

        H ->
            7


getRank : Int -> Rank
getRank index =
    case index of
        0 ->
            One

        1 ->
            Two

        2 ->
            Three

        3 ->
            Four

        4 ->
            Five

        5 ->
            Six

        6 ->
            Seven

        7 ->
            Eight

        _ ->
            Eight


getFile : Int -> File
getFile index =
    case index of
        0 ->
            A

        1 ->
            B

        2 ->
            C

        3 ->
            D

        4 ->
            E

        5 ->
            F

        6 ->
            G

        7 ->
            H

        _ ->
            H
