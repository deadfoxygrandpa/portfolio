module Menu (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects
import String


type Action
    = Close
    | Update String String
    | Submit


type alias Model =
    { project : PartialProject
    }


type alias PartialProject =
    { title : Maybe String
    , subtitle : Maybe String
    , date : Maybe String
    , category : String
    , w : Maybe Float
    , h : Maybe Float
    }


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )


emptyPartialProject : PartialProject
emptyPartialProject =
    PartialProject Nothing Nothing Nothing "" Nothing Nothing


init : Model
init =
    Model emptyPartialProject


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
    case action of
        Update field value ->
            let
                partialProject = model.project

                newProject =
                    case field of
                        "title" ->
                            { partialProject | title = Just value }

                        "subtitle" ->
                            { partialProject | subtitle = Just value }

                        "date" ->
                            { partialProject | date = Just value }

                        "category" ->
                            { partialProject | category = value }

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
                ( { model | project = newProject }, Effects.none )

        _ ->
            ( model, Effects.none )


view : Signal.Address Action -> Model -> Html
view address model =
    let
        makeLink heading string ref =
            heading [] [ Html.a [ Html.Attributes.href ref ] [ text string ] ]
    in
        Html.div
            [ Html.Attributes.class "menu"
            , Html.Attributes.style
                [ "position" => "fixed"
                , "top" => "0px"
                , "left" => "0px"
                , "width" => "100%"
                , "height" => "100%"
                , "backgroundColor" => "rgba(0, 0, 0, 0.8)"
                , "zIndex" => "997"
                ]
            ]
            [ Html.div
                [ Html.Attributes.style
                    [ "position" => "fixed"
                    , "top" => "50%"
                    , "left" => "50%"
                    , "transform" => "translate(-50%, -60%)"
                    ]
                ]
                [ makeLink Html.h1 "HOME" "#home"
                , makeLink Html.h1 "PROJECTS" "#projects"
                , makeLink Html.h1 "ABOUT ME" "#about"
                , makeLink Html.h1 "CONTACT" "#contact"
                ]
            , Html.div
                [ style
                    [ "position" => "fixed"
                    , "bottom" => "3em"
                    , "left" => "50%"
                    , "transform" => "translateX(-50%)"
                    ]
                ]
                [ Html.h2
                    []
                    [ text "FOLLOW ME" ]
                , Html.div
                    [ Html.Attributes.style
                        [ "display" => "inline-block"
                        ]
                    ]
                    [ makeLink Html.h2 "Facebook" "#facebook"
                    , Html.h2 [] [ text "|" ]
                    , makeLink Html.h2 "Sina Weibo" "#weibo"
                    , Html.h2 [] [ text "|" ]
                    , makeLink Html.h2 "Instagram" "#instagram"
                    , Html.h2 [] [ text "|" ]
                    , makeLink Html.h2 "Behance" "#behance"
                    ]
                ]
            ]



--div
--    [ style
--        [ "position" => "fixed"
--        , "top" => "200px"
--        , "right" => "100px"
--        , "width" => "150px"
--        , "display" => "inline-block"
--        ]
--    ]
--    [ h5 [ style [ "width" => "150px" ] ] [ text "Add New Project" ]
--    , input [ on "input" targetValue (Signal.message address << Update "title") ] []
--    , input [ on "input" targetValue (Signal.message address << Update "subtitle") ] []
--    , input [ on "input" targetValue (Signal.message address << Update "date") ] []
--    , select
--        [ on "change" targetValue (Signal.message address << Update "category") ]
--        [ option [] [ text "--" ]
--        , option [] [ text "Interface" ]
--        , option [] [ text "UX" ]
--        , option [] [ text "Graphic" ]
--        , option [] [ text "Photograph" ]
--        ]
--    , input [ on "input" targetValue (Signal.message address << Update "width") ] []
--    , input [ on "input" targetValue (Signal.message address << Update "height") ] []
--    , h5
--        [ onClick address Submit
--        , style
--            [ "width" => "40px"
--            , "display" => "inline-block"
--            , "cursor" => "pointer"
--            ]
--        ]
--        [ text "Add" ]
--    , h5
--        [ onClick address Close
--        , style
--            [ "width" => "40px"
--            , "display" => "inline-block"
--            , "position" => "absolute"
--            , "right" => "0px"
--            , "cursor" => "pointer"
--            ]
--        ]
--        [ text "Close" ]
--    ]
