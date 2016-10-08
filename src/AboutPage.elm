module AboutPage exposing (Model, Action, init, update, view)

import Html exposing (Html)
import Html.Attributes
import Effects
import ColorScheme
import Header
import Json.Decode
import Task
import Markdown
import ImageLoad


-- Model


type alias Model =
    { header : Header.Model
    , loading : List String
    }


init : ( Model, Effects.Effects Action )
init =
    let
        images =
            [ "gigi1.jpg" ]

        model =
            { header = Header.init "ABOUT ME"
            , loading = images
            }
    in
        ( model, Effects.tick (always LoadImage) )



-- Update


type Action
    = HeaderAction Header.Action
    | LoadImage


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
    case action of
        HeaderAction headerAction ->
            let
                ( newHeader, fx ) =
                    Header.update headerAction model.header
            in
                ( { model | header = newHeader }, Effects.map HeaderAction fx )

        LoadImage ->
            let
                fx =
                    List.head model.loading
                        |> Maybe.map loadImage
                        |> Maybe.withDefault Effects.none

                loading =
                    List.tail model.loading |> Maybe.withDefault []
            in
                ( { model | loading = loading }, fx )


loadImage : String -> Effects.Effects Action
loadImage image =
    ImageLoad.load ("assets/" ++ image) (Json.Decode.succeed image)
        `Task.onError` always (Task.succeed image)
        |> Task.map (always LoadImage)
        |> Effects.task



-- View


view : Signal.Address Action -> Model -> Html
view address model =
    case model.loading of
        [] ->
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
                        , "backgroundColor" => "#ebebeb"
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
                        [ Html.Attributes.src "assets/gigi1.jpg"
                        , Html.Attributes.style
                            [ "width" => "100%"
                            , "height" => "550px"
                            , "position" => "relative"
                            , "left" => "50%"
                            , "transform" => "translateX(-50%)"
                            ]
                        ]
                        []
                    , Html.h1
                        [ Html.Attributes.style
                            [ "textAlign" => "center"
                            , "position" => "relative"
                            , "top" => "-2.5em"
                            , "color" => "#FFFFFF"
                            ]
                        ]
                        [ Html.text "THANKS FOR VISITING" ]
                    , Html.span
                        [ Html.Attributes.style
                            [ "color" => "#000000"
                            , "fontSize" => "1em"
                            , "position" => "relative"
                            , "top" => "-5em"
                            ]
                        ]
                        [ textContent ]
                    ]
                ]

        _ ->
            Html.text "loading..."


textContent : Html.Html
textContent =
    Markdown.toHtml
        """Hi, I am Gigi, User experience & Graphic designer based in Shenzhen, China.
I create a more useful and beautiful product from the UIUX skills that I had during serveral years of experience.

Nice to meet you!

I am a passionate girl both in life and work. It's not just because I am always very energetic but also because I love and enjoy my life at every moment.
I grow up in a small town in Guangzhou, China, which helps me to keep myself close to nature/local environment. After studing in big cities for 7 years, I still think the love of culture and nature from childhood keep pushing me forward.

Travelling is the way to know myself better, what I experience from the trips makes me think outside of the box and see myself and the world differently. That helps me to keep a fresh mind on design jobs and daily life, as well as continually extending my horizon of the world."""


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
