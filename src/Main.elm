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
import Result
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
    , pendingProject : PartialProject
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
        emptyPartialProject


type Action
    = Click Category
    | Hover (Maybe Category)
    | AddProject Project
    | RemoveProject ID
    | UpdatePartialProject String String
    | SubmitNewProject



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

        UpdatePartialProject field value ->
            let
                partialProject = model.pendingProject

                newProject =
                    case field of
                        "title" ->
                            { partialProject | title = Just value }

                        "subtitle" ->
                            { partialProject | subtitle = Just value }

                        "date" ->
                            { partialProject | date = Just value }

                        "category" ->
                            let
                                newCategory =
                                    case value of
                                        "Interface" ->
                                            Just Interface

                                        "UX" ->
                                            Just UX

                                        "Graphic" ->
                                            Just Graphic

                                        "Photograph" ->
                                            Just Photograph

                                        _ ->
                                            Nothing
                            in
                                { partialProject | category = newCategory }

                        "width" ->
                            case String.toFloat value of
                                Result.Ok f ->
                                    { partialProject | w = Just f }

                                Result.Err _ ->
                                    { partialProject | w = Nothing }

                        "height" ->
                            case String.toFloat value of
                                Result.Ok f ->
                                    { partialProject | h = Just f }

                                Result.Err _ ->
                                    { partialProject | h = Nothing }

                        _ ->
                            partialProject
            in
                ( { model | pendingProject = newProject }, none )

        SubmitNewProject ->
            let
                andThen = Maybe.andThen

                newProject =
                    model.pendingProject.title
                        `andThen` \title ->
                                    model.pendingProject.subtitle
                                        `andThen` \subtitle ->
                                                    model.pendingProject.date
                                                        `andThen` \date ->
                                                                    model.pendingProject.category
                                                                        `andThen` \category ->
                                                                                    model.pendingProject.w
                                                                                        `andThen` \w ->
                                                                                                    model.pendingProject.h
                                                                                                        `andThen` \h ->
                                                                                                                    (Just <| Project title subtitle date category w h)
            in
                case newProject of
                    Just project ->
                        update (AddProject project) { model | pendingProject = emptyPartialProject }

                    Nothing ->
                        ( model, none )


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


addProjectForm : Signal.Address Action -> Html
addProjectForm address =
    div [] []


type alias Project =
    { title : String
    , subtitle : String
    , date : String
    , category : Category
    , w : Float
    , h : Float
    }


type alias PartialProject =
    { title : Maybe String
    , subtitle : Maybe String
    , date : Maybe String
    , category : Maybe Category
    , w : Maybe Float
    , h : Maybe Float
    }


emptyPartialProject : PartialProject
emptyPartialProject =
    PartialProject Nothing Nothing Nothing Nothing Nothing Nothing


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
