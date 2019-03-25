module Board exposing (Board, BoardIndex, File(..), Move, MoveError(..), Piece(..), Player(..), Position, PositionError(..), Rank(..), SquareState(..), init)

import Array exposing (Array, set)


type Board
    = Board Squares


type alias Squares =
    Array SquareState


init : List ( Piece, Position ) -> Board
init initPositions =
    let
        emptySquares =
            Array.repeat 64 Free
    in
    Board (occupySquares initPositions emptySquares)


occupySquares : List ( Piece, Position ) -> Squares -> Squares
occupySquares pieces squares =
    case pieces of
        ( piece, pos ) :: [] ->
            set (unpack (toIndex pos)) (Taken piece) squares

        ( piece, pos ) :: tail ->
            set (unpack (toIndex pos)) (Taken piece) (occupySquares tail)


unpack : BoardIndex -> Int
unpack (BoardIndex index) =
    index


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


type Player
    = Black
    | White


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


type Move
    = Move
        { from : BoardIndex
        , to : BoardIndex
        , board : Board
        }


type MoveError
    = StaleMove


type PositionError
    = OffBoard


getValidMoves : Player -> Board -> Piece -> List Move


executeMove : Board -> Move -> Result MoveError Board
executeMove board (Move move) =
    if move.board == board then
        case board of
            Board squares ->
                Ok (Debug.todo "Update both from position and to position in the board")

    else
        Err StaleMove


addRank : Board -> Int -> BoardIndex -> Result PositionError Move
addRank board ranks (BoardIndex index) =
    let
        newIndex =
            index + ranks

        oldFile =
            toFile index

        newFile =
            toFile newIndex
    in
    if oldFile == newFile then
        Ok
            (Move
                { board = board
                , from = BoardIndex index
                , to = BoardIndex newIndex
                }
            )

    else
        Err OffBoard


addFile : Board -> Int -> BoardIndex -> Result PositionError Move
addFile board files (BoardIndex index) =
    let
        newIndex =
            index + files * boardSize

        oldRank =
            toRank index

        newRank =
            toRank newIndex
    in
    if oldRank == newRank then
        Ok
            (Move
                { board = board
                , from = BoardIndex index
                , to = BoardIndex newIndex
                }
            )

    else
        Err OffBoard


type alias Position =
    ( File, Rank )


type BoardIndex
    = BoardIndex Int


toIndex : Position -> BoardIndex
toIndex ( file, rank ) =
    BoardIndex (toFileNumeral file * boardSize + toRankNumeral rank)


boardSize =
    8


toPosition : BoardIndex -> Position
toPosition (BoardIndex index) =
    let
        rank =
            modBy boardSize index

        file =
            round (toFloat (index - rank) / toFloat boardSize)
    in
    ( toFile file, toRank rank )


toRankNumeral : Rank -> Int
toRankNumeral rank =
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


toFileNumeral : File -> Int
toFileNumeral file =
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


toRank : Int -> Rank
toRank index =
    let
        clamped =
            clamp 0 7 index
    in
    case clamped of
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


toFile : Int -> File
toFile index =
    let
        clamped =
            clamp 0 7 index
    in
    case clamped of
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
