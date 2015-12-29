module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (..)
import Svg
import Svg.Attributes
import Material.Icons.Social
import Material.Icons.Navigation
import Task
import Color
import String
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
            , "fontFamily" => "Raleway,sans-serif"
            , "maxWidth" => "75em"
            , "margin" => "auto"
            , "backgroundColor" => "#f2f9fa"
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
        [ style [ "position" => "absolute" ] ]
        [ div
            [ style
                [ "position" => "absolute"
                , "top" => "46px"
                , "left" => "51px"
                ]
            ]
            [ Svg.svg
                [ Svg.Attributes.width "50px"
                , Svg.Attributes.height "50px"
                ]
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
                [ Svg.Attributes.width "50px"
                , Svg.Attributes.height "50px"
                ]
                [ Material.Icons.Social.public (Color.rgb 80 227 194) 50 ]
            ]
        ]


hamburger : Html
hamburger =
    div
        [ style
            [ "position" => "relative"
            , "width" => "100%"
            ]
        ]
        [ div
            [ style
                [ "position" => "absolute"
                , "top" => "35px"
                , "right" => "35px"
                , "cursor" => "pointer"
                ]
            ]
            [ Svg.svg
                [ Svg.Attributes.width "50px"
                , Svg.Attributes.height "50px"
                ]
                [ Material.Icons.Navigation.menu (Color.rgb 74 74 74) 50 ]
            ]
        ]


title' : Html
title' =
    h1
        [ style
            [ "position" => "relative"
            , "top" => "41px"
            , "textAlign" => "center"
            ]
        ]
        [ text "PROJECTS" ]


selectors : Signal.Address Action -> Model -> Html
selectors address model =
    let
        styler : Category -> Html
        styler category =
            h2
                [ classList [ "selected" => (category == model.current) ]
                , style
                    [ "display" => "inherit"
                    , "padding" => "15px"
                    , "cursor" => "pointer"
                    , "backgroundColor"
                        => if model.hovered == (Just category) then
                            "#ffffff"
                           else
                            "inherit"
                    ]
                , onClick address (Click category)
                , onMouseOver address (Hover (Just category))
                , onMouseLeave address (Hover Nothing)
                ]
                [ text (category |> toString >> String.toUpper) ]
    in
        div
            [ style
                [ "textAlign" => "center"
                , "display" => "inline-block"
                , "width" => "100%"
                ]
            ]
            [ styler All
            , styler Interface
            , styler UX
            , styler Graphic
            , styler Photograph
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
                        , "bottom" => "20px"
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
