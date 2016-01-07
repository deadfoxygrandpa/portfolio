module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
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
import Menu


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
    , menu : Menu.Model
    , menuOpen : Bool
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
        Menu.init
        False


type Action
    = Click Category
    | Hover (Maybe Category)
    | AddProject Project
    | RemoveProject ID
    | SubmitNewProject
    | OpenMenu
    | CloseMenu
    | ToggleMenu
    | MenuAction Menu.Action


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

        SubmitNewProject ->
            let
                newProject = convertPartialProject model.menu.project
            in
                case newProject of
                    Just project ->
                        update (AddProject project) model

                    Nothing ->
                        ( model, none )

        OpenMenu ->
            ( { model | menuOpen = True }, none )

        CloseMenu ->
            ( { model | menuOpen = False }, none )

        ToggleMenu ->
            if model.menuOpen then
                update CloseMenu model
            else
                update OpenMenu model

        MenuAction menuAction ->
            let
                ( newMenu, effect ) = Menu.update menuAction model.menu

                updated = { model | menu = newMenu }
            in
                case menuAction of
                    Menu.Close ->
                        update CloseMenu updated

                    Menu.Submit ->
                        update SubmitNewProject updated

                    _ ->
                        ( updated, none )


view : Signal.Address Action -> Model -> Html
view address model =
    div
        [ style
            [ "color" => "#50e3c2"
            , "fontFamily" => "Raleway,sans-serif"
            , "maxWidth" => "75em"
            , "margin" => "auto"
            ]
        ]
        <| [ logo
           , lazy2 hamburger address model.menuOpen
           , title'
           , lazy2 selectors address ( model.current, model.hovered )
           , lazy2 projects address ( model.current, model.projects )
           ]
        ++ if model.menuOpen then
            [ Menu.view (Signal.forwardTo address MenuAction) model.menu ]
           else
            []


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


hamburger : Signal.Address Action -> Bool -> Html
hamburger address open =
    let
        icon =
            if open then
                Material.Icons.Navigation.close (Color.rgb 255 255 255)
            else
                Material.Icons.Navigation.menu (Color.rgb 74 74 74)
    in
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
                    , "zIndex" => "998"
                    ]
                , onClick address ToggleMenu
                ]
                [ div
                    [ style
                        [ "position" => "absolute"
                        , "top" => "0px"
                        , "left" => "0px"
                        , "width" => "50px"
                        , "height" => "50px"
                        , "zIndex" => "999"
                        ]
                    ]
                    []
                , Svg.svg
                    [ Svg.Attributes.width "50px"
                    , Svg.Attributes.height "50px"
                    ]
                    [ icon 50 ]
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


selectors : Signal.Address Action -> ( Category, Maybe Category ) -> Html
selectors address ( current, hovered ) =
    let
        styler : Category -> Html
        styler category =
            h2
                [ classList [ "selected" => (category == current) ]
                , style
                    [ "display" => "inherit"
                    , "padding" => "15px"
                    , "cursor" => "pointer"
                    , "backgroundColor"
                        => if hovered == (Just category) then
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


convertPartialProject : Menu.PartialProject -> Maybe Project
convertPartialProject project =
    let
        andThen = Maybe.andThen

        readCategory =
            case project.category of
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
        project.title
            `andThen` \title ->
                        project.subtitle
                            `andThen` \subtitle ->
                                        project.date
                                            `andThen` \date ->
                                                        readCategory
                                                            `andThen` \category ->
                                                                        project.w
                                                                            `andThen` \w ->
                                                                                        project.h
                                                                                            `andThen` \h ->
                                                                                                        (Just <| Project title subtitle date category w h)


projects : Signal.Address Action -> ( Category, List ( ID, Project ) ) -> Html
projects address ( current, projectList ) =
    div
        [ style
            [ "position" => "relative"
            ]
        ]
        <| List.map
            (viewProject address)
        <| List.filter
            (\( _, project ) -> project.category == current || current == All)
            projectList


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
