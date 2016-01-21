module AboutPage (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Effects
import ColorScheme
import Header


type alias Model =
    { header : Header.Model }


type Action
    = HeaderAction Header.Action


init : Model
init =
    { header = Header.init "ABOUT ME" ColorScheme.highlight2 }


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
    case action of
        HeaderAction headerAction ->
            let
                ( newHeader, effect ) = Header.update headerAction model.header
            in
                ( { model | header = newHeader }, Effects.map HeaderAction effect )


view : Signal.Address Action -> Model -> Html
view address model =
    Html.div
        []
        [ Header.view (Signal.forwardTo address HeaderAction) model.header
        , h1 [] [ text "THANKS FOR VISITING" ]
        ]
