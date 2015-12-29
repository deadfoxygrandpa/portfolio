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
    , projects : List ( ID, Project )
    , nextID : Int
    }


type alias ID =
    Int


model : Model
model =
    Model
        All
        Nothing
        [ ( 0, Project "JBL CONNECT" "APP" "JULY,2015" UX 541.4 412 )
        , ( 1, Project "GIGI" "WEBSITE" "DEC,2015" Interface 776 412 )
        , ( 2, Project "BREATH" "POSTER" "DEC,2011" Graphic 310 412 )
        , ( 3, Project "FUTURE" "POSTER" "DEC,2013" Graphic 310 412 )
        , ( 4, Project "HORIZON" "UX" "JULY,2014" Photograph 670 130 )
        ]
        5


type Action
    = Click Category
    | Hover (Maybe Category)
    | AddProject Project
    | RemoveProject ID



--update : Action -> a -> ( Model, Effects Action )


update action model =
    case action of
        Click category ->
            ( { model | current = category, hovered = Nothing }, none )

        Hover category ->
            ( { model | hovered = category }, none )

        AddProject project ->
            ( { model
                | projects = ( model.nextID, project ) :: model.projects
                , nextID = model.nextID + 1
              }
            , none
            )

        RemoveProject id ->
            ( { model | projects = List.filter (\( pid, _ ) -> pid /= id) model.projects }, none )


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
        , projects address model
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


type alias Project =
    { title : String
    , subtitle : String
    , date : String
    , category : Category
    , w : Float
    , h : Float
    }


projects : Signal.Address Action -> Model -> Html
projects address model =
    div
        [ style
            [ "position" => "relative"
            ]
        ]
        <| List.map
            (viewProject address)
        <| List.filter
            (\( _, project ) -> project.category == model.current || model.current == All)
            model.projects


viewProject : Signal.Address Action -> ( ID, Project ) -> Html
viewProject address ( id, project ) =
    div
        [ style
            [ "width" => (toString project.w ++ "px")
            , "height" => (toString project.h ++ "px")
            , "backgroundColor" => "#ffffff"
            , "color" => "#333333"
            , "fontFamily" => "monospace"
            , "position" => "relative"
            , "left" => "40px"
            , "display" => "inline-block"
            , "margin" => "11px"
            ]
        , onClick address (RemoveProject id)
        ]
        [ div
            [ style
                [ "position" => "absolute"
                , "bottom" => "20px"
                , "left" => "40px"
                ]
            ]
            [ h3 [] [ text <| "<" ++ project.title ++ ">" ]
            , h4 [] [ text <| project.subtitle ++ " - " ++ project.date ]
            ]
        ]
