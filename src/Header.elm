module Header (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Effects
import Svg
import Svg.Attributes
import SvgIcon
import ColorScheme exposing (..)
import Menu
import Graphics.Collage as Collage


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )


type alias Model =
    { title : String
    , menu : Menu.Model
    }


type Action
    = MenuAction Menu.Action


init : String -> Model
init title =
    Model title Menu.init


view : Signal.Address Action -> Model -> Html
view address model =
    Html.header
        []
        [ logo
        , title' model.title
        , Menu.view (Signal.forwardTo address MenuAction) model.menu
        ]


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
    case action of
        MenuAction menuAction ->
            let
                ( newMenu, effect ) = Menu.update menuAction model.menu
            in
                ( { model | menu = newMenu }, Effects.map MenuAction effect )


logo : Html
logo =
    Html.div
        []
        [ Html.text "English"
        , Html.text "|"
        , Html.text "中文"
        ]


title' : String -> Html
title' title =
    h1
        []
        [ text title ]


sub =
    Collage.collage 300 300 []
