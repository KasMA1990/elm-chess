module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Chessboard exposing (Board, newBoard)
import Html exposing (Html, table)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Board


init : Model
init =
    newBoard



-- UPDATE


type Msg
    = Increment


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    table []
        []
