module ContactPage (Model, Action, init, update, view) where

import Html exposing (Html)
import Html.Attributes
import Effects
import ColorScheme
import Header


-- Model


type alias Model =
  { header : Header.Model }


init : Model
init =
  Model
    (Header.init "CONTACT")



-- Update


type Action
  = HeaderAction Header.Action


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    HeaderAction headerAction ->
      let
        ( newHeader, fx ) =
          Header.update headerAction model.header
      in
        ( { model | header = newHeader }, Effects.map HeaderAction fx )



-- View


view : Signal.Address Action -> Model -> Html
view address model =
  Html.div
    []
    [ Html.div
        [ Html.Attributes.style
            [ "position" => "fixed"
            , "top" => "0px"
            , "left" => "0px"
            , "zIndex" => "-999"
            , "width" => "100%"
            , "height" => "100%"
            , "backgroundColor" => "#faf5f2"
            ]
        ]
        []
    , Header.view (Signal.forwardTo address HeaderAction) model.header
    , Html.div
        [ Html.Attributes.style
            [ "width" => "1100px"
            , "position" => "relative"
            , "left" => "50%"
            , "transform" => "translateX(-50%)"
            ]
        ]
        [ bar
        , spacer "2.5em"
        , Html.img
            [ Html.Attributes.src "assets/contact.png"
            , Html.Attributes.style
                [ "width" => "150px"
                , "height" => "94px"
                , "position" => "relative"
                , "left" => "50%"
                , "transform" => "translateX(-50%)"
                ]
            ]
            []
        , Html.h1
            [ Html.Attributes.style
                [ "textAlign" => "center"
                , "marginTop" => "0px"
                , "fontSize" => "2.5em"
                ]
            ]
            [ Html.text "Get in touch" ]
        , Html.div
            [ Html.Attributes.class "flex-container"
            , Html.Attributes.style
                [ "flexDirection" => "column"
                , "alignItems" => "center"
                , "width" => "75%"
                , "position" => "relative"
                , "left" => "50%"
                , "transform" => "translateX(-50%)"
                ]
            ]
            [ Html.span
                [ Html.Attributes.class "contact" ]
                [ Html.text "Your name" ]
            , Html.input
                [ Html.Attributes.placeholder "Enter your name" ]
                []
            , Html.span
                [ Html.Attributes.class "contact" ]
                [ Html.text "Your contact email" ]
            , Html.input
                [ Html.Attributes.placeholder "Enter your email address" ]
                []
            , Html.span
                [ Html.Attributes.class "contact" ]
                [ Html.text "Your email will deliver straight away to my inbox" ]
            , Html.textarea
                [ Html.Attributes.placeholder "Enter your message" ]
                []
            , Html.button
                []
                [ Html.text "SEND" ]
            ]
        ]
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
