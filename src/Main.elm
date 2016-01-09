module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Effects exposing (..)
import Task
import String
import Random
import StartApp
import Header
import ColorScheme
import Scroll


app : StartApp.App Model
app =
    StartApp.start { init = ( model, none ), view = view, update = update, inputs = [ Signal.map Offset Scroll.yOffset ] }


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
    , projects : List Project
    , header : Header.Model
    , scrollOffset : Int
    }


model : Model
model =
    Model
        All
        Nothing
        [ Project UX 562 412
        , Project UX 776 412
        , Project Interface 335 247
        , Project Graphic 312 247
        , Project Graphic 692 247
        , Project Photograph 975 130
        , Project Photograph 363 130
        ]
        Header.init
        0


type Action
    = Click Category
    | Hover (Maybe Category)
    | HeaderAction Header.Action
    | Offset Int


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
    case action of
        Click category ->
            ( { model | current = category, hovered = Nothing }, none )

        Hover category ->
            ( { model | hovered = category }, none )

        HeaderAction headerAction ->
            let
                ( newHeader, effect ) = Header.update headerAction model.header
            in
                ( { model | header = newHeader }, Effects.map HeaderAction effect )

        Offset offset ->
            ( { model | scrollOffset = offset }, none )


view : Signal.Address Action -> Model -> Html
view address model =
    div
        [ style
            [ "color" => "#50e3c2"
            , "fontFamily" => "Raleway,sans-serif"
            , "maxWidth" => "90em"
            , "margin" => "auto"
            ]
        ]
        <| [ Header.view (Signal.forwardTo address HeaderAction) model.header
           , lazy2 selectors address ( model.current, model.hovered )
           , lazy2 projects address ( model.current, model.projects )
           ]


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )


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
    { category : Category
    , w : Float
    , h : Float
    }


projects : Signal.Address Action -> ( Category, List Project ) -> Html
projects address ( current, projectList ) =
    let
        seed = Random.initialSeed 12014591

        ( colors, _ ) = Random.generate (Random.list 100 ColorScheme.randomColor) seed
    in
        div
            [ style
                [ "position" => "relative"
                ]
            ]
            <| List.map2
                (viewProject address)
                colors
            <| List.filter
                (\project -> project.category == current || current == All)
                projectList


viewProject : Signal.Address Action -> ColorScheme.Color -> Project -> Html
viewProject address color project =
    div
        [ style
            [ "width" => (toString project.w ++ "px")
            , "height" => (toString project.h ++ "px")
            , "backgroundColor" => color.hex
            , "color" => "#333333"
            , "fontFamily" => "monospace"
            , "position" => "relative"
            , "left" => "40px"
            , "display" => "inline-block"
            , "margin" => "0px"
            ]
        ]
        [ div
            [ style
                [ "position" => "absolute"
                , "bottom" => "20px"
                , "left" => "40px"
                ]
            ]
            []
        ]
