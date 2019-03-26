module Board exposing (Board, File(..), Move, MoveError(..), Piece(..), Player(..), Position, Rank(..), SquareState(..), blackInit, executeMove, get, getValidMoves, init, whiteInit)

-- I have attempted to build the Board module to be a general chess board that different clients could use with a sensible interface.
-- In this case the Chess module is the client

import Array exposing (Array, set)


type Board
    = Board Squares



-- I have chosen to represent my board in an array, since I will be doing a lot of reads;
-- for example, to determine valid moves. It is also nice if I want to let the (unimplemented) view function work with slices
-- for rows for example


type alias Squares =
    Array SquareState



-- Any list of pieces and positions can be loaded, allowing the caller to decide to load a game with fresh positions,
-- or a list of pieces from a prior unfinished game maybe. Also makes it easier to test


init : List ( Player, Piece, Position ) -> Board
init initPositions =
    let
        emptySquares =
            Array.repeat 64 Free
    in
    Board (occupySquares initPositions emptySquares)


occupySquares : List ( Player, Piece, Position ) -> Squares -> Squares
occupySquares pieces squares =
    case pieces of
        [] ->
            squares

        ( player, piece, pos ) :: [] ->
            set (unpack (toIndex pos)) (Taken ( player, piece )) squares

        ( player, piece, pos ) :: tail ->
            set (unpack (toIndex pos)) (Taken ( player, piece )) (occupySquares tail squares)


unpack : BoardIndex -> Int
unpack (BoardIndex index) =
    index


get : Position -> Board -> SquareState
get pos board =
    getByIndex (toIndex pos) board


getByIndex : BoardIndex -> Board -> SquareState
getByIndex (BoardIndex index) (Board squares) =
    -- Free is used as a default value, though it should never be used in practice;
    -- Position is constrained, so as to only match a valid index in the array
    Maybe.withDefault Free (Array.get index squares)


type SquareState
    = Free
    | Taken ( Player, Piece )


type Piece
    = King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn


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


getValidMoves : Player -> Board -> List ( Piece, List Move )
getValidMoves player board =
    let
        (Board squares) =
            board

        playerPositions =
            Array.toIndexedList squares
                |> List.filterMap (toPlayerSquare player)
    in
    List.map (\( index, piece ) -> ( piece, getValidMovesForPiece board index piece )) playerPositions


getValidMovesForPiece : Board -> BoardIndex -> Piece -> List Move
getValidMovesForPiece board index piece =
    -- TODO Determine actual valid moves for each piece
    List.filterMap Result.toMaybe [ addRank board 1 index ]


toPlayerSquare : Player -> ( Int, SquareState ) -> Maybe ( BoardIndex, Piece )
toPlayerSquare player ( index, square ) =
    case square of
        Free ->
            Nothing

        Taken ( occupantPlayer, piece ) ->
            if occupantPlayer == player then
                Just ( BoardIndex index, piece )

            else
                Nothing



-- I only want to allow executing a Move on the board it was constructed from, so the move contains a reference to its board.
-- In Rust, I could have made it a compile time error to execute a stale move, instead of a run time error,
-- since Rust allows me to consume data of certain types completely, forcing the caller to pass in the necessary data
-- without letting them keep references to it


executeMove : Board -> Move -> Result MoveError Board
executeMove board move =
    let
        (Move actualMove) =
            move
    in
    if actualMove.board == board then
        case board of
            Board squares ->
                let
                    ( player, piece ) =
                        case getByIndex actualMove.from board of
                            Taken x ->
                                x

                            -- TODO Move types can only be constructed from squares that are Taken, and I already verified
                            -- that the Move is based on the current board, so this case should be impossible. How to best handle it?
                            Free ->
                                Debug.todo "How to handle this best?"

                    boardWithLiftedPiece =
                        updateFromPosition squares move
                in
                Ok (Board (updateToPosition player piece boardWithLiftedPiece move))

    else
        Err StaleMove


updateToPosition : Player -> Piece -> Squares -> Move -> Squares
updateToPosition player piece squares (Move move) =
    set (unpack move.to) (Taken ( player, piece )) squares


updateFromPosition : Squares -> Move -> Squares
updateFromPosition squares (Move move) =
    set (unpack move.from) Free squares


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



-- Human readable type to be used for the public interface when users want to deal with positions and not just moves


type alias Position =
    ( File, Rank )



-- Internal new-type around Int, to make sure the indices don't get mixed with other numbers in the program


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

        _ ->
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

        _ ->
            H


whiteInit : List ( Player, Piece, Position )
whiteInit =
    [ ( White, Rook, ( A, One ) )
    , ( White, Knight, ( B, One ) )
    , ( White, Bishop, ( C, One ) )
    , ( White, Queen, ( D, One ) )
    , ( White, King, ( E, One ) )
    , ( White, Bishop, ( F, One ) )
    , ( White, Knight, ( G, One ) )
    , ( White, Rook, ( H, One ) )
    , ( White, Pawn, ( A, Two ) )
    , ( White, Pawn, ( B, Two ) )
    , ( White, Pawn, ( C, Two ) )
    , ( White, Pawn, ( D, Two ) )
    , ( White, Pawn, ( E, Two ) )
    , ( White, Pawn, ( F, Two ) )
    , ( White, Pawn, ( G, Two ) )
    , ( White, Pawn, ( H, Two ) )
    ]


blackInit : List ( Player, Piece, Position )
blackInit =
    [ ( Black, Rook, ( A, Eight ) )
    , ( Black, Knight, ( B, Eight ) )
    , ( Black, Bishop, ( C, Eight ) )
    , ( Black, Queen, ( D, Eight ) )
    , ( Black, King, ( E, Eight ) )
    , ( Black, Bishop, ( F, Eight ) )
    , ( Black, Knight, ( G, Eight ) )
    , ( Black, Rook, ( H, Eight ) )
    , ( Black, Pawn, ( A, Seven ) )
    , ( Black, Pawn, ( B, Seven ) )
    , ( Black, Pawn, ( C, Seven ) )
    , ( Black, Pawn, ( D, Seven ) )
    , ( Black, Pawn, ( E, Seven ) )
    , ( Black, Pawn, ( F, Seven ) )
    , ( Black, Pawn, ( G, Seven ) )
    , ( Black, Pawn, ( H, Seven ) )
    ]
