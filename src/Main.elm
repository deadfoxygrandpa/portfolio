module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Element exposing (..)
import Color

import StartApp.Simple as StartApp


-- design inspired by http://www.thomsoon.com/thomsoon/projects.html


main =
  StartApp.start { model = model, view = view, update = update }



port title : String
port title =
  "Gigi's Portfolio?"


model = 
  False


update action model =
  case action of
    _ -> not model


view address model =
  div 
    [ style 
      [ ("fontFamily", "\"Raleway\", sans-serif") 
      , ("color", "black")
      , ("position", "relative")
      ] 
    ] 
    [ hamburger address model
    , fullscreenMenu address model
    , logo
    , body
    , footer
    , node "link"
      [ rel "stylesheet"
      , href "https://fonts.googleapis.com/css?family=Raleway:400,700,900,800"
      , type' "text/css"
      ]
      []
    ]


logo =
  div 
    [ style 
      [ ("cursor", "pointer") 
      , ("fontSize", "20px")
      , ("fontWeight", "800")
      , ("lineHeight", "80px")
      , ("width", "400px")
      , ("margin", "0px auto")
      ] 
    ] 
    [ text "GIGI"
    , spacer 150 1 |> color Color.lightGrey |> fromElement
    , node "link" 
      [ rel "stylesheet"
      , href "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css"
      ]
      []
    ]
    

body =
  div 
    [ style 
      [ ("fontSize", "13px")
      , ("color", "#8D8D8D")
      , ("lineHeight", "22px")
      , ("letterSpacing", "1px")
      , ("width", "400px")
      , ("margin", "50px auto")
      ]
    ] 
    [ h1 
      [ style 
        [ ("fontSize", "43px")
        , ("fontWeight", "900")
        , ("lineHeight", "70px")
        , ("color", "black")
        , ("letterSpacing", "0px")
        ]
      ] 
      [ text "PROJECTS" ]
    , text "Please check my portfolio."
    , pic Color.grey
    , pic Color.lightGrey
    , pic Color.grey
    ]


hamburger address model =
  node
    "i"
    [ class <| if model then "fa fa-close" else "fa fa-bars"
    , style 
      [ ("backgroundColor", "black") 
      , ("position", "fixed")
      , ("right", "5%")
      , ("top", "30px")
      , ("padding", "20px")
      , ("color", "white")
      , ("fontSize", "18px")
      , ("cursor", "pointer")
      , ("zIndex", "9999")
      ]
    , onClick address ()
    ]
    []
    

fullscreenMenu address model =
  if model then
    div
      [ style
        [ ("position", "fixed")
        , ("height", "100%")
        , ("width", "100%")
        , ("background", "rgba(0, 0, 0, 0.95)")
        , ("zIndex", "9998")
        ]
      ]
      [ text "welp," ]
  else
    div [] []


footer =
  div
    [ style 
      [ ("height", "300px")
      , ("marginTop", "150px")
      , ("background", "#F1F1F1")
      , ("color", "#686868")
      , ("letterSpacing", "1px")
      , ("fontSize", "12px")
      , ("textAlign", "center")
      ]
    ]
    [ divSpacer 150
    , text "© Copyright 2015 deadfoxygrandpa.xyz. All Rights Reserved." 
    ]


pic c =
  p [] [ spacer 360 305 |> color c |> fromElement ]
  
  
divSpacer h =
  div
    [ style
      [ ("height", toString h ++ "px")
      , ("width", "100%")
      ]
    ]
    []
  