module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Chess exposing (GameState, init)
import Html exposing (Html, table)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    GameState


init : Model
init =
    Chess.init



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
