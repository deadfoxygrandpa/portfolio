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
  , hovered : Maybe Category
  , projects : List Project
  }


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
    [ Project "screen-shot-2016-01-09-at-103012-p-m.png" UX 535 412
    , Project "1-intro.png" UX 543 412
    , Project "screen-shot-2015-10-02-at-112628-am.png" Interface 712 387
    , Project "screen-shot-2016-01-09-at-103618-p-m.png" Graphic 365 387
    , Project "trees-breath.png" Graphic 221 301
    , Project "2.png" Photograph 221 301
    , Project "untitled-1.png" Photograph 221 301
    , Project "m-2-b.png" Graphic 415 301
    ]



-- Update


type Action
  = Click Category
  | Hover (Maybe Category)
  | HeaderAction Header.Action


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    Click category ->
      ( { model | current = category, hovered = Nothing }, Effects.none )

    Hover category ->
      ( { model | hovered = category }, Effects.none )

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
    , Html.Lazy.lazy2 selectors address ( model.current, model.hovered )
    , Html.Lazy.lazy2 projects address ( model.current, model.projects )
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
selectors address ( current, hovered ) =
  let
    styler : Category -> Html
    styler category =
      Html.h2
        [ Html.Attributes.classList
            [ "selected" => (category == current)
            , "selector" => True
            ]
        , Html.Events.onClick address (Click category)
        , Html.Events.onMouseOver address (Hover (Just category))
        , Html.Events.onMouseLeave address (Hover Nothing)
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


projects : Signal.Address Action -> ( Category, List Project ) -> Html
projects address ( current, projectList ) =
  let
    seed =
      Random.initialSeed 12014591

    ( colors, _ ) =
      Random.generate (Random.list 100 ColorScheme.randomColor) seed
  in
    Html.div
      [ Html.Attributes.class "flex-container" ]
      <| List.map2
          (viewProject address)
          colors
      <| List.filter
          (\project -> project.category == current || current == All)
          projectList


viewProject : Signal.Address Action -> ColorScheme.Color -> Project -> Html
viewProject address color project =
  Html.div
    []
    [ Html.img
        [ Html.Attributes.style
            [ "width" => (toString project.w ++ "px")
            , "height" => (toString project.h ++ "px")
            ]
        , Html.Attributes.src ("assets/" ++ project.image)
        ]
        []
    ]
