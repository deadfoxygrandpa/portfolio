module ProjectPage (Model, Action, init, update, view) where

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Task
import Effects
import Header
import ColorScheme
import Random
import String


-- Model


type alias Model =
  { header : Header.Model
  , current : Category
  , hoveredCategory : Maybe Category
  , hoveredProject : Maybe ID
  , projects : List ( ID, Project )
  }


type alias ID =
  Int


type Category
  = All
  | Interface
  | UX
  | Graphic
  | Photograph


type alias Project =
  { image : String
  , category : Category
  , w : Float
  , h : Float
  }


init : Model
init =
  Model
    (Header.init "PROJECTS")
    All
    Nothing
    Nothing
    [ ( 0, Project "screen-shot-2016-01-09-at-103012-p-m.png" UX 535 412 )
    , ( 1, Project "1-intro.png" UX 543 412 )
    , ( 2, Project "screen-shot-2015-10-02-at-112628-am.png" Interface 712 387 )
    , ( 3, Project "screen-shot-2016-01-09-at-103618-p-m.png" Graphic 365 387 )
    , ( 4, Project "trees-breath.png" Graphic 221 301 )
    , ( 5, Project "2.png" Photograph 221 301 )
    , ( 6, Project "untitled-1.png" Photograph 221 301 )
    , ( 7, Project "m-2-b.png" Graphic 415 301 )
    ]



-- Update


type Action
  = Click Category
  | HoverCategory (Maybe Category)
  | HoverProject (Maybe ID)
  | HeaderAction Header.Action


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    Click category ->
      ( { model | current = category, hoveredCategory = Nothing }, Effects.none )

    HoverCategory category ->
      ( { model | hoveredCategory = category }, Effects.none )

    HoverProject id ->
      ( { model | hoveredProject = id }, Effects.none )

    HeaderAction headerAction ->
      let
        ( newHeader, fx ) =
          Header.update headerAction model.header
      in
        ( { model | header = newHeader }, Effects.map HeaderAction fx )



-- View


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div
    []
    [ Header.view (Signal.forwardTo address HeaderAction) model.header
    , bar
    , Html.Lazy.lazy2 selectors address ( model.current, model.hoveredCategory )
    , Html.Lazy.lazy2 projects address ( model.current, model.projects, model.hoveredProject )
    ]


bar : Html
bar =
  Html.div
    [ Html.Attributes.class "bar" ]
    []


spacer : String -> Html
spacer size =
  Html.div
    [ Html.Attributes.class "spacer"
    , Html.Attributes.style [ "height" => size ]
    ]
    []


(=>) : a -> b -> ( a, b )
(=>) a b =
  ( a, b )


selectors : Signal.Address Action -> ( Category, Maybe Category ) -> Html
selectors address ( current, hoveredCategory ) =
  let
    styler : Category -> Html
    styler category =
      Html.h2
        [ Html.Attributes.classList
            [ "selected" => (category == current)
            , "selector" => True
            , "hovered" => (Just category == hoveredCategory)
            ]
        , Html.Events.onClick address (Click category)
        , Html.Events.onMouseOver address (HoverCategory (Just category))
        , Html.Events.onMouseLeave address (HoverCategory Nothing)
        ]
        [ Html.text (category |> toString >> String.toUpper) ]
  in
    Html.div
      [ Html.Attributes.classList
          [ "flex-container" => True
          , "selectors" => True
          ]
      ]
      [ styler All
      , styler Interface
      , styler UX
      , styler Graphic
      , styler Photograph
      ]


projects : Signal.Address Action -> ( Category, List ( ID, Project ), Maybe ID ) -> Html
projects address ( current, projectList, hoveredProject ) =
  Html.div
    [ Html.Attributes.class "flex-container" ]
    <| List.map
        (viewProject address hoveredProject)
    <| List.filter
        (\( id, project ) -> project.category == current || current == All)
        projectList


viewProject : Signal.Address Action -> Maybe ID -> ( ID, Project ) -> Html
viewProject address hoveredProject ( id, project ) =
  Html.div
    []
    [ Html.div
        [ Html.Attributes.style
            [ "width" => (toString project.w ++ "px")
            , "height" => (toString project.h ++ "px")
            , "background" => ("url(" ++ "assets/" ++ project.image ++ ") no-repeat")
            ]
        , Html.Events.onMouseOver address (HoverProject (Just id))
        , Html.Events.onMouseLeave address (HoverProject Nothing)
        ]
        [ Html.div
            [ Html.Attributes.style
                [ "width" => (toString project.w ++ "px")
                , "height" => (toString project.h ++ "px")
                , "opacity"
                    => (if hoveredProject == Just id then
                          "0.8"
                        else
                          "0.0"
                       )
                ]
            , Html.Attributes.class "hoverproject"
            ]
            []
        ]
    ]
