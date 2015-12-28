module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (..)
import Task
import StartApp


app : StartApp.App Model
app =
    StartApp.start { init = ( model, none ), view = view, update = update, inputs = [] }


main : Signal Html
main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks


port title : String
port title =
    "Gg"


type Model
    = All
    | Interface
    | UX
    | Graphic
    | Photograph


model : Model
model =
    All


type Action
    = Click Model


update : Action -> a -> ( Model, Effects Action )
update action model =
    case action of
        Click model' ->
            ( model', none )


view : Signal.Address Action -> Model -> Html
view address model =
    div
        []
        [ text "welp," ]


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )
