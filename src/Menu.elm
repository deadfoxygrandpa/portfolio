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
import Transit
import TransitStyle


type Action
    = Close
    | Open
    | Toggle
    | TransitAction (Transit.Action Action)


type alias Model =
    Transit.WithTransition { open : Bool }


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )


init : Model
init =
    { open = False, transition = Transit.initial }


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
    case action of
        Close ->
            ( { model | open = False }, Effects.none )

        Open ->
            ( { model | open = True }, Effects.none )

        Toggle ->
            let
                timeline =
                    if model.open then
                        Transit.timeline 100 Close 200
                    else
                        Transit.timeline 100 Open 500
            in
                Transit.init TransitAction timeline model

        TransitAction a ->
            Transit.update TransitAction a model


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
            heading [] [ Html.a [ Html.Attributes.href ref, Html.Attributes.target target ] [ text string ] ]
    in
        Html.div
            [ Html.Attributes.class "menu"
            , Html.Attributes.style
                <| [ "position" => "fixed"
                   , "top" => "0px"
                   , "left" => "0px"
                   , "width" => "100%"
                   , "height" => "100%"
                   , "backgroundColor" => "rgba(0, 0, 0, 0.8)"
                   , "zIndex" => "997"
                   ]
                ++ (TransitStyle.fade model.transition)
            ]
            [ Html.div
                [ Html.Attributes.style
                    [ "position" => "fixed"
                    , "top" => "50%"
                    , "left" => "50%"
                    , "transform" => "translate(-50%, -60%)"
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
            [ style
                [ "position" => "relative"
                , "width" => "100%"
                ]
            ]
            [ div
                [ style
                    [ "position" => "absolute"
                    , "top" => "-45px"
                    , "right" => "35px"
                    , "cursor" => "pointer"
                    , "zIndex" => "998"
                    , "transform" => ("translateY(" ++ toString yOffset ++ "px)")
                    ]
                , onClick address Toggle
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
