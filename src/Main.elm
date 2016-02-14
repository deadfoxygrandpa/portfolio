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
import Hop
import AboutPage
import Graphics.Element as E
import ProjectPage
import AboutPage


app : StartApp.App Model
app =
  StartApp.start { init = init, view = view, update = update, inputs = [ router.signal ] }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


port title : String
port title =
  "Gg"


port hop : Task.Task () ()
port hop =
  router.run


type alias Model =
  { projects : ProjectPage.Model
  , about : AboutPage.Model
  , routerPayload : Hop.Payload
  , currentView : String
  }


model : Model
model =
  { projects = ProjectPage.init
  , about = AboutPage.init
  , routerPayload = router.payload
  , currentView = ""
  }


init : ( Model, Effects Action )
init =
  ( model, Effects.none )


type Action
  = ProjectsAction ProjectPage.Action
  | AboutMeAction AboutPage.Action
  | HopAction Hop.Action
  | ShowNotFound Hop.Payload
  | ShowProjectsPage Hop.Payload
  | ShowAboutMePage Hop.Payload


routes : List ( String, Hop.Payload -> Action )
routes =
  [ "/" => ShowProjectsPage
  , "projects" => ShowProjectsPage
  , "about" => ShowAboutMePage
  ]


router : Hop.Router Action
router =
  Hop.new
    { routes = routes
    , notFoundAction = ShowNotFound
    }


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    ProjectsAction projectsAction ->
      let
        ( newPage, fx ) =
          ProjectPage.update projectsAction model.projects
      in
        ( { model | projects = newPage }, Effects.map ProjectsAction fx )

    AboutMeAction aboutAction ->
      let
        ( newPage, fx ) =
          AboutPage.update aboutAction model.about
      in
        ( { model | about = newPage }, Effects.map AboutMeAction fx )

    HopAction hopAction ->
      ( model, Effects.none )

    ShowNotFound payload ->
      ( { model | routerPayload = payload }, Effects.none )

    ShowProjectsPage payload ->
      ( { model | currentView = "projects", routerPayload = payload }, Effects.none )

    ShowAboutMePage payload ->
      ( { model | currentView = "about", routerPayload = payload }, Effects.none )


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ Html.div
        [ Html.Attributes.class "content" ]
        [ case Debug.log "view" model.currentView of
            "projects" ->
              ProjectPage.view (Signal.forwardTo address ProjectsAction) model.projects

            "about" ->
              AboutPage.view (Signal.forwardTo address AboutMeAction) model.about

            _ ->
              ProjectPage.view (Signal.forwardTo address ProjectsAction) model.projects
        ]
    , footer
    ]


footer =
  let
    makeLink string ref =
      Html.h2
        []
        [ Html.a [ Html.Attributes.href ref ] [ Html.text string ] ]
  in
    Html.footer
      []
      [ Html.span [] [ Html.text "Copyright Gigi 2016 © All Rights Reserved." ]
      , Html.span [] [ Html.text "------------------------------------------------------" ]
      , Html.div
          [ Html.Attributes.class "flex-container" ]
          [ makeLink "Facebook" "https://www.facebook.com/profile.php?id=100002934303779"
          , Html.h2 [] [ text "|" ]
          , makeLink "Sina Weibo" "http://www.weibo.com/u/1829414713?wvr=5&wvr=5&lf=reg"
          , Html.h2 [] [ text "|" ]
          , makeLink "Instagram" "https://www.instagram.com/gigiguo80/"
          , Html.h2 [] [ text "|" ]
          , makeLink "Behance" "https://www.behance.net/GigiGUO"
          ]
      , Html.span [] [ Html.text "Made by Alex" ]
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
