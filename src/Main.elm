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
import Transit
import TransitRouter
import Graphics.Element as E


app : StartApp.App Model
app =
    StartApp.start { init = init "@_@", view = view, update = update, inputs = [ Signal.map Offset Scroll.yOffset, Signal.map RouterAction TransitRouter.actions ] }


main : Signal Html
main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks


port title : String
port title =
    "Gg"


type Category
    = All
    | Interface
    | UX
    | Graphic
    | Photograph


type alias Page =
    { current : Category
    , hovered : Maybe Category
    , projects : List Project
    , header : Header.Model
    , scrollOffset : Int
    , route : Route
    }


type alias Model =
    TransitRouter.WithRoute Route { page : Page }


model : Model
model =
    let
        page =
            Page
                All
                Nothing
                [ Project UX 562 412
                , Project UX 776 412
                , Project Interface 335 247
                , Project Graphic 312 247
                , Project Graphic 692 247
                , Project Photograph 975 130
                , Project Photograph 363 130
                ]
                (Header.init "PROJECTS" ColorScheme.highlight1)
                0
                Projects
    in
        { page = page, transitRouter = TransitRouter.empty Projects }


init : String -> ( Model, Effects Action )
init path =
    TransitRouter.init routerConfig path model


type Action
    = Click Category
    | Hover (Maybe Category)
    | HeaderAction Header.Action
    | Offset Int
    | RouterAction (TransitRouter.Action Route)


type Route
    = Projects
    | AboutMe
    | NotFound String


routerConfig : TransitRouter.Config Route Action Model
routerConfig =
    let
        mountRoute : Route -> Route -> Model -> ( Model, Effects Action )
        mountRoute old new model =
            let
                page = model.page

                header = page.header

                menu = header.menu

                closedMenu = { header | menu = { menu | open = False } }
            in
                ( updatePage { page | route = new, header = closedMenu } model, Effects.none )

        getDurations : Route -> Route -> Model -> ( Float, Float )
        getDurations old new model =
            ( 50, 100 )

        actionWrapper : TransitRouter.Action Route -> Action
        actionWrapper action =
            RouterAction action

        routeDecoder : String -> Route
        routeDecoder route =
            case route of
                "#home" ->
                    Projects

                "#projects" ->
                    Projects

                "#about" ->
                    AboutMe

                _ ->
                    NotFound route
    in
        { mountRoute = mountRoute
        , getDurations = getDurations
        , actionWrapper = actionWrapper
        , routeDecoder = routeDecoder
        }


updatePage : Page -> Model -> Model
updatePage page model =
    { model | page = page }


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
    let
        page = model.page
    in
        case action of
            Click category ->
                ( { model | page = { page | current = category, hovered = Nothing } }, none )

            Hover category ->
                ( { model | page = { page | hovered = category } }, none )

            HeaderAction headerAction ->
                let
                    ( newHeader, effect ) = Header.update headerAction page.header
                in
                    ( { model | page = { page | header = newHeader } }, Effects.map HeaderAction effect )

            Offset offset ->
                ( { model | page = { page | scrollOffset = offset } }, none )

            RouterAction routeAction ->
                TransitRouter.update routerConfig routeAction model


view : Signal.Address Action -> Model -> Html
view address model =
    div
        [ style
            [ "color" => "#50e3c2"
            , "fontFamily" => "Raleway,sans-serif"
            , "maxWidth" => "90em"
            , "margin" => "auto"
            ]
        ]
        [ if TransitRouter.getRoute model == AboutMe then
            (aboutMePage address model)
          else
            (projectPage address model)
        ]


projectPage address model =
    Html.div
        []
        [ Header.view (Signal.forwardTo address HeaderAction) model.page.header
        , lazy2 selectors address ( model.page.current, model.page.hovered )
        , lazy2 projects address ( model.page.current, model.page.projects )
        ]


aboutMePage address model =
    let
        bar =
            Html.div
                [ Html.Attributes.style
                    [ "width" => "100%"
                    , "height" => "2px"
                    , "backgroundColor" => "#000000"
                    , "opacity" => "0.1"
                    ]
                ]
                []

        spacer s =
            Html.div
                [ Html.Attributes.style
                    [ "width" => "100%"
                    , "height" => s
                    ]
                ]
                []
    in
        Html.div
            []
            [ Html.div
                [ style
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
            , Header.view (Signal.forwardTo address HeaderAction) (Header.init "ABOUT ME" ColorScheme.highlight2)
            , Html.div
                [ style
                    [ "width" => "1100px"
                    , "position" => "relative"
                    , "left" => "50%"
                    , "transform" => "translateX(-50%)"
                    ]
                ]
                [ spacer "4em"
                , bar
                , Html.h1
                    [ Html.Attributes.style
                        [ "textAlign" => "center"
                        , "fontWeight" => "200"
                        , "color" => "#4a4a4a"
                        ]
                    ]
                    [ text "THANKS FOR VISITING" ]
                , bar
                , spacer "3.5em"
                , Html.img
                    [ Html.Attributes.src "assets/gigi1.jpg"
                    , style
                        [ "width" => "100%"
                        , "height" => "550px"
                        , "position" => "relative"
                        , "left" => "50%"
                        , "transform" => "translateX(-50%)"
                        ]
                    ]
                    []
                , spacer "3.5em"
                , Html.span
                    [ style
                        [ "color" => "#000000"
                        , "fontSize" => "1em"
                        ]
                    ]
                    [ text
                        """Hi, I am Gigi, User experience & Graphic designer based in Shenzhen, China.
I create a more useful and beautiful product from the UIUX skills that I had during serveral years of experience.

Nice to meet you!
I am a passionate girl both in life and work. It's not just because I am always very energetic but also because I love and enjoy my life at every moment.
I grow up in a small town in Guangzhou, China, which helps me to keep myself close to nature/local environment. After studing in big cities for 7 years, I still think the love of culture and nature from childhood keep pushing me forward.

Travelling is the way to know myself better, what I experience from the trips makes me think outside of the box and see myself and the world differently. That helps me to keep a fresh mind on design jobs and daily life, as well as continually extending my horizon of the world."""
                    ]
                ]
            ]


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )


selectors : Signal.Address Action -> ( Category, Maybe Category ) -> Html
selectors address ( current, hovered ) =
    let
        styler : Category -> Html
        styler category =
            h2
                [ classList [ "selected" => (category == current) ]
                , style
                    [ "display" => "inherit"
                    , "padding" => "15px"
                    , "cursor" => "pointer"
                    ]
                , onClick address (Click category)
                , onMouseOver address (Hover (Just category))
                , onMouseLeave address (Hover Nothing)
                ]
                [ text (category |> toString >> String.toUpper) ]
    in
        div
            [ style
                [ "textAlign" => "center"
                , "display" => "inline-block"
                , "width" => "100%"
                ]
            ]
            [ styler All
            , styler Interface
            , styler UX
            , styler Graphic
            , styler Photograph
            ]


type alias Project =
    { category : Category
    , w : Float
    , h : Float
    }


projects : Signal.Address Action -> ( Category, List Project ) -> Html
projects address ( current, projectList ) =
    let
        seed = Random.initialSeed 12014591

        ( colors, _ ) = Random.generate (Random.list 100 ColorScheme.randomColor) seed
    in
        div
            [ style
                [ "position" => "relative"
                ]
            ]
            <| List.map2
                (viewProject address)
                colors
            <| List.filter
                (\project -> project.category == current || current == All)
                projectList


viewProject : Signal.Address Action -> ColorScheme.Color -> Project -> Html
viewProject address color project =
    div
        [ style
            [ "width" => (toString project.w ++ "px")
            , "height" => (toString project.h ++ "px")
            , "backgroundColor" => color.hex
            , "color" => "#333333"
            , "fontFamily" => "monospace"
            , "position" => "relative"
            , "left" => "40px"
            , "display" => "inline-block"
            , "margin" => "0px"
            ]
        ]
        [ div
            [ style
                [ "position" => "absolute"
                , "bottom" => "20px"
                , "left" => "40px"
                ]
            ]
            []
        ]
