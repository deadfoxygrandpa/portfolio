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
import AboutPage
import Graphics.Element as E
import ProjectPage
import AboutPage


app : StartApp.App Model
app =
  StartApp.start { init = init, view = view, update = update, inputs = [] }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


port title : String
port title =
  "Gg"


type Page
  = Projects ProjectPage.Model
  | AboutMe AboutPage.Model


type alias Model =
  { page : Page
  }


model : Model
model =
  let
    page =
      Projects ProjectPage.init
  in
    { page = page }


init : ( Model, Effects Action )
init =
  ( model, Effects.none )


type Action
  = ProjectsAction ProjectPage.Action
  | AboutMeAction AboutPage.Action


updatePage : Page -> Model -> Model
updatePage page model =
  { model | page = page }


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    ProjectsAction projectsAction ->
      case model.page of
        AboutMe _ ->
          ( model, Effects.none )

        Projects page ->
          let
            ( newPage, fx ) =
              ProjectPage.update projectsAction page
          in
            ( { model | page = Projects newPage }, Effects.map ProjectsAction fx )

    AboutMeAction aboutAction ->
      case model.page of
        Projects _ ->
          ( model, Effects.none )

        AboutMe page ->
          let
            ( newPage, fx ) =
              AboutPage.update aboutAction page
          in
            ( { model | page = AboutMe newPage }, Effects.map AboutMeAction fx )


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ Html.div
        [ Html.Attributes.class "content" ]
        [ case model.page of
            Projects page ->
              ProjectPage.view (Signal.forwardTo address ProjectsAction) page

            AboutMe page ->
              AboutPage.view (Signal.forwardTo address AboutMeAction) page
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
