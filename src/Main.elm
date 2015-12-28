module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (..)
import Svg
import Material.Icons.Social
import Material.Icons.Navigation
import Task
import Color
import StartApp


app : StartApp.App Model
app =
    StartApp.start { init = ( model, none ), view = view, update = update, inputs = [] }


main : Signal Html
main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks


port title : String
port title =
    "Gg"


type Model
    = All
    | Interface
    | UX
    | Graphic
    | Photograph


model : Model
model =
    All


type Action
    = Click Model


update : Action -> a -> ( Model, Effects Action )
update action model =
    case action of
        Click model' ->
            ( model', none )


view : Signal.Address Action -> Model -> Html
view address model =
    div
        [ style
            [ "color" => "#50e3c2"
            , "fontFamily" => "sans-serif"
            ]
        ]
        [ logo
        , title'
        , hamburger
        ]


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )


logo : Html
logo =
    div
        []
        [ div
            [ style
                [ "position" => "absolute"
                , "top" => "46px"
                , "left" => "51px"
                ]
            ]
            [ Svg.svg
                []
                [ Material.Icons.Social.public (Color.rgb 248 231 28) 50 ]
            ]
        , div
            [ style
                [ "position" => "absolute"
                , "top" => "55.8px"
                , "left" => "94.6px"
                ]
            ]
            [ Svg.svg
                []
                [ Material.Icons.Social.public (Color.rgb 80 227 194) 50 ]
            ]
        ]


title' : Html
title' =
    h1
        [ style
            [ "top" => "41px"
            , "textAlign" => "center"
            ]
        ]
        [ text "PROJECTS" ]


hamburger : Html
hamburger =
    div
        [ style
            [ "position" => "fixed"
            , "top" => "61px"
            , "right" => "50px"
            , "width" => "50px"
            , "height" => "30px"
            ]
        ]
        [ Svg.svg
            []
            [ Material.Icons.Navigation.menu (Color.rgb 74 74 74) 50 ]
        ]
