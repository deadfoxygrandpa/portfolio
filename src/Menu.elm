module Menu (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy
import Effects
import Material.Icons.Navigation
import Svg
import Svg.Attributes
import Color
import Json.Decode as Json


type Action
  = Close
  | Open
  | Toggle


type alias Model =
  { open : Bool }


(=>) : a -> b -> ( a, b )
(=>) a b =
  ( a, b )


clickTo : String -> List Attribute
clickTo path =
  [ href path
  ]


init : Model
init =
  { open = False }


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    Close ->
      ( { model | open = False }, Effects.none )

    Open ->
      ( { model | open = True }, Effects.none )

    Toggle ->
      ( { model | open = not model.open }, Effects.none )


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ Html.Lazy.lazy2 hamburger address ( model.open, 0 )
    , if model.open then
        menu address model
      else
        div [] []
    ]


menu : Signal.Address Action -> Model -> Html
menu address model =
  let
    makeLink heading string ref target =
      heading [ Html.Events.onClick address Close ] [ Html.a (clickTo ref) [ text string ] ]
  in
    Html.div
      [ Html.Attributes.class "menu"
      ]
      [ Html.div
          [ Html.Attributes.classList
              [ "flex-container" => True
              , "centered-column" => True
              , "screen-center" => True
              ]
          ]
          [ makeLink Html.h1 "HOME" "#home" ""
          , makeLink Html.h1 "PROJECTS" "#projects" ""
          , makeLink Html.h1 "ABOUT ME" "#about" ""
          , makeLink Html.h1 "CONTACT" "#contact" ""
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
              [ makeLink Html.h2 "Facebook" "https://www.facebook.com/profile.php?id=100002934303779" "_blank"
              , Html.h2 [] [ text "|" ]
              , makeLink Html.h2 "Sina Weibo" "http://www.weibo.com/u/1829414713?wvr=5&wvr=5&lf=reg" "_blank"
              , Html.h2 [] [ text "|" ]
              , makeLink Html.h2 "Instagram" "https://www.instagram.com/gigiguo80/" "_blank"
              , Html.h2 [] [ text "|" ]
              , makeLink Html.h2 "Behance" "https://www.behance.net/GigiGUO" "_blank"
              ]
          ]
      ]


hamburger : Signal.Address Action -> ( Bool, Int ) -> Html
hamburger address ( open, yOffset ) =
  let
    icon =
      if open then
        Material.Icons.Navigation.close (Color.rgb 255 255 255)
      else
        Material.Icons.Navigation.menu (Color.rgb 74 74 74)
  in
    div
      [ Html.Attributes.class "menutoggle"
      , onClick address Toggle
      ]
      [ Svg.svg
          [ Svg.Attributes.width "50px"
          , Svg.Attributes.height "50px"
          ]
          [ icon 50 ]
      ]
