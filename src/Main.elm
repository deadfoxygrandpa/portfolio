module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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


type Category
    = All
    | Interface
    | UX
    | Graphic
    | Photograph


type alias Model =
    { current : Category
    , hovered : Maybe Category
    }


model : Model
model =
    Model All Nothing


type Action
    = Click Category
    | Hover (Maybe Category)



--update : Action -> a -> ( Model, Effects Action )


update action model =
    case action of
        Click category ->
            ( Model category Nothing, none )

        Hover category ->
            ( { model | hovered = category }, none )


view : Signal.Address Action -> Model -> Html
view address model =
    div
        [ style
            [ "color" => "#50e3c2"
            , "fontFamily" => "sans-serif"
            , "maxWidth" => "1440px"
            , "margin" => "auto"
            ]
        ]
        [ logo
        , hamburger
        , title'
        , selectors address model
        , project "JBL CONNECT" "APP" "JULY,2015" UX 541.4 412 model
        , project "GIGI" "WEBSITE" "DEC,2015" Interface 776 412 model
        , project "BREATH" "POSTER" "DEC,2011" Graphic 310 412 model
        , project "FUTURE" "POSTER" "DEC,2013" Graphic 310 412 model
        , project "HORIZON" "UX" "JULY,2014" Photograph 670 130 model
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
            , "cursor" => "pointer"
            ]
        ]
        [ Svg.svg
            []
            [ Material.Icons.Navigation.menu (Color.rgb 74 74 74) 50 ]
        ]


selectors : Signal.Address Action -> Model -> Html
selectors address model =
    let
        styler : Category -> List Html.Attribute
        styler category =
            [ classList [ "selected" => (category == model.current) ]
            , style
                [ "display" => "inherit"
                , "padding" => "15px"
                ]
            , onClick address (Click category)
            , onMouseOver address (Hover (Just category))
            , onMouseLeave address (Hover Nothing)
            ]
    in
        div
            [ style
                [ "textAlign" => "center"
                , "display" => "inline-block"
                , "width" => "100%"
                , "cursor" => "pointer"
                ]
            ]
            [ h2
                (styler All)
                [ text "ALL" ]
            , h2
                (styler Interface)
                [ text "INTERFACE" ]
            , h2
                (styler UX)
                [ text "UX" ]
            , h2
                (styler Graphic)
                [ text "GRAPHIC" ]
            , h2
                (styler Photograph)
                [ text "PHOTOGRAPH" ]
            ]


project : String -> String -> String -> Category -> Float -> Float -> Model -> Html
project title subtitle date category w h model =
    let
        rendered =
            div
                [ style
                    [ "width" => (toString w ++ "px")
                    , "height" => (toString h ++ "px")
                    , "backgroundColor"
                        => if model.hovered == (Just category) || model.hovered == (Just All) then
                            "#ffffff"
                           else if model.hovered == Nothing then
                            "#ffffff"
                           else
                            "#eeeeee"
                    , "color" => "#333333"
                    , "fontFamily" => "monospace"
                    , "position" => "relative"
                    , "left" => "40px"
                    , "display" => "inline-block"
                    , "margin" => "11px"
                    ]
                ]
                [ div
                    [ style
                        [ "position" => "absolute"
                        , "bottom" => "40px"
                        , "left" => "40px"
                        ]
                    ]
                    [ h3 [] [ text <| "<" ++ title ++ ">" ]
                    , h4 [] [ text <| subtitle ++ " - " ++ date ]
                    ]
                ]
    in
        case model.current of
            All ->
                rendered

            _ ->
                if category == model.current then
                    rendered
                else
                    div [] []
