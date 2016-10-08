module Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
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
    , language : Language
    }


type Language
    = English
    | Chinese


type Action
    = ChangeLanguage Language
    | MenuAction Menu.Action


init : String -> Model
init title =
    Model title Menu.init English


view : Signal.Address Action -> Model -> Html
view address model =
    Html.header
        []
        [ languageSelector address model.language
        , title' model.title
        , Menu.view (Signal.forwardTo address MenuAction) model.menu
        ]


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
    case action of
        ChangeLanguage language ->
            ( { model | language = language }, Effects.none )

        MenuAction menuAction ->
            let
                ( newMenu, effect ) =
                    Menu.update menuAction model.menu
            in
                ( { model | menu = newMenu }, Effects.map MenuAction effect )


languageSelector : Signal.Address Action -> Language -> Html
languageSelector address language =
    Html.div
        [ Html.Attributes.class "languageselector" ]
        [ Html.span
            [ Html.Attributes.classList
                [ "language" => True
                , "selectedlanguage" => (language == English)
                ]
            , Html.Events.onClick address (ChangeLanguage English)
            ]
            [ Html.text "English" ]
        , Html.span [ Html.Attributes.class "language" ] [ Html.text "|" ]
        , Html.span
            [ Html.Attributes.classList
                [ "language" => True
                , "selectedlanguage" => (language == Chinese)
                ]
            , Html.Events.onClick address (ChangeLanguage Chinese)
            ]
            [ Html.text "中文" ]
        ]


title' : String -> Html
title' title =
    h1
        []
        [ text title ]


sub =
    Collage.collage 300 300 []
