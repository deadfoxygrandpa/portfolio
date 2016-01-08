module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Effects exposing (..)
import Svg
import Svg.Attributes
import Material.Icons.Social
import Material.Icons.Navigation
import Task
import Color
import String
import Result
import StartApp
import Transit
import Menu
import SvgIcon


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


type Category
    = All
    | Interface
    | UX
    | Graphic
    | Photograph


type alias Model =
    Transit.WithTransition { page : Page }


type alias Page =
    { current : Category
    , hovered : Maybe Category
    , projects : List ( ID, Project )
    , nextID : Int
    , menu : Menu.Model
    , menuOpen : Bool
    }


type alias ID =
    Int


model : Model
model =
    let
        page =
            Page
                All
                Nothing
                [ ( 0, Project "JBL CONNECT" "APP" "JULY,2015" UX 541.4 412 )
                , ( 1, Project "GIGI" "WEBSITE" "DEC,2015" Interface 776 412 )
                , ( 2, Project "BREATH" "POSTER" "DEC,2011" Graphic 310 412 )
                , ( 3, Project "FUTURE" "POSTER" "DEC,2013" Graphic 310 412 )
                , ( 4, Project "HORIZON" "UX" "JULY,2014" Photograph 670 130 )
                ]
                5
                Menu.init
                False
    in
        { page = page, transition = Transit.initial }


type Action
    = Click Category
    | Hover (Maybe Category)
    | AddProject Project
    | RemoveProject ID
    | SubmitNewProject
    | OpenMenu
    | CloseMenu
    | ToggleMenu
    | MenuAction Menu.Action
    | TransitAction (Transit.Action Action)


update action rootModel =
    let
        model = rootModel.page
    in
        case action of
            Click category ->
                ( { rootModel | page = { model | current = category, hovered = Nothing } }, none )

            Hover category ->
                ( { rootModel | page = { model | hovered = category } }, none )

            AddProject project ->
                ( { rootModel
                    | page =
                        { model
                            | projects = ( model.nextID, project ) :: model.projects
                            , nextID = model.nextID + 1
                        }
                  }
                , none
                )

            RemoveProject id ->
                ( { rootModel | page = { model | projects = List.filter (\( pid, _ ) -> pid /= id) model.projects } }, none )

            SubmitNewProject ->
                let
                    newProject = convertPartialProject model.menu.project
                in
                    case newProject of
                        Just project ->
                            update (AddProject project) rootModel

                        Nothing ->
                            ( rootModel, none )

            OpenMenu ->
                ( { rootModel | page = { model | menuOpen = True } }, none )

            CloseMenu ->
                ( { rootModel | page = { model | menuOpen = False } }, none )

            ToggleMenu ->
                if model.menuOpen then
                    update CloseMenu rootModel
                else
                    update OpenMenu rootModel

            MenuAction menuAction ->
                let
                    ( newMenu, effect ) = Menu.update menuAction model.menu

                    updated = { rootModel | page = { model | menu = newMenu } }
                in
                    case menuAction of
                        Menu.Close ->
                            update CloseMenu updated

                        Menu.Submit ->
                            update SubmitNewProject updated

                        _ ->
                            ( updated, none )

            TransitAction a ->
                Transit.update TransitAction a rootModel


view : Signal.Address Action -> Model -> Html
view address rootModel =
    let
        model = rootModel.page
    in
        div
            [ style
                [ "color" => "#50e3c2"
                , "fontFamily" => "Raleway,sans-serif"
                , "maxWidth" => "75em"
                , "margin" => "auto"
                ]
            ]
            <| [ logo
               , lazy2 hamburger address model.menuOpen
               , title'
               , lazy2 selectors address ( model.current, model.hovered )
               , lazy2 projects address ( model.current, model.projects )
               ]
            ++ if model.menuOpen then
                [ Menu.view (Signal.forwardTo address MenuAction) model.menu ]
               else
                []


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )


logo : Html
logo =
    div
        [ style [ "position" => "absolute" ] ]
        [ Html.a
            [ style
                [ "position" => "absolute"
                , "top" => "46px"
                , "left" => "51px"
                , "zIndex" => "997"
                ]
            , Html.Attributes.href "#home"
            ]
            [ Svg.svg
                [ Svg.Attributes.width "80px"
                , Svg.Attributes.height "60px"
                ]
                [ SvgIcon.icon2
                    "M41.6717791,45.6595092 C39.8108291,46.6786009 37.4625227,47.6090619 34.6267894,48.4509202 C31.791056,49.2927786 28.5122885,49.7137014 24.7903885,49.7137014 C21.0684885,49.7137014 17.6789519,49.0712402 14.6216769,47.7862986 C11.5644019,46.5013569 8.95024944,44.7401271 6.7791411,42.5025562 C4.60803277,40.2649854 2.93541828,37.6286791 1.76124744,34.5935583 C0.58707661,31.5584374 0,28.268593 0,24.7239264 C0,21.1792597 0.598153528,17.8894153 1.79447853,14.8542945 C2.99080353,11.8191736 4.66341802,9.20502119 6.81237219,7.01175869 C8.96132635,4.81849619 11.5422481,3.10157403 14.5552147,1.8609407 C17.5681814,0.620307362 20.8912565,0 24.5245399,0 C27.7147399,0 30.639046,0.487384356 33.297546,1.46216769 C35.956046,2.43695102 38.3043525,3.76618108 40.3425358,5.44989775 L39.2791411,6.51329243 C38.3929744,5.6714341 37.3628211,4.92928065 36.1886503,4.28680982 C35.0144795,3.64433898 33.7738647,3.10157004 32.4667689,2.65848671 C31.1596731,2.21540337 29.8193661,1.88309586 28.4458078,1.66155419 C27.0722494,1.44001253 25.7430194,1.32924335 24.4580777,1.32924335 C21.0020277,1.32924335 17.8561832,1.9384738 15.0204499,3.15695297 C12.1847166,4.37543213 9.7588717,6.04804663 7.74284254,8.17484663 C5.72681337,10.3016466 4.17604496,12.7828761 3.0904908,15.6186094 C2.00493663,18.4543427 1.46216769,21.489418 1.46216769,24.7239264 C1.46216769,27.9584347 1.99385971,31.0156639 3.05725971,33.8957055 C4.12065971,36.7757472 5.6603512,39.2902074 7.67638037,41.4391616 C9.69240953,43.5881157 12.1514851,45.282884 15.053681,46.5235174 C17.9558768,47.7641507 21.2457212,48.3844581 24.9233129,48.3844581 C28.1135129,48.3844581 30.9492037,48.0521506 33.4304703,47.3875256 C35.911737,46.7229006 38.193582,45.8810549 40.2760736,44.8619632 L40.2760735,33.8957062 L18.816178,33.8957062 L18.8161791,17.9340317 L41.6717801,17.9340317 L41.6717791,45.6595092 Z"
                    "M76.8007271,33.1431493 L76.6013406,33.1431493 C76.0253323,34.6496326 75.2499481,35.9677858 74.2751647,37.0976483 C73.3003814,38.2275108 72.2148435,39.1801256 71.0185185,39.9555215 C69.8221935,40.7309173 68.5261942,41.3179939 67.1304817,41.7167689 C65.7347692,42.1155439 64.3058469,42.3149284 62.8436719,42.3149284 C60.6282552,42.3149284 58.5790255,41.9050825 56.6959214,41.0853783 C54.8128172,40.2656742 53.1955873,39.1358286 51.8441831,37.6958078 C50.492779,36.2557869 49.4404718,34.5499417 48.6872302,32.5782209 C47.9339885,30.6065 47.5573733,28.468655 47.5573733,26.1646217 C47.5573733,23.8605883 47.9339885,21.7116664 48.6872302,19.7177914 C49.4404718,17.7239164 50.492779,15.9959173 51.8441831,14.5337423 C53.1955873,13.0715673 54.8128172,11.9306449 56.6959214,11.1109407 C58.5790255,10.2912365 60.6282552,9.88139059 62.8436719,9.88139059 C65.7680219,9.88139059 68.4597128,10.6567748 70.9188253,12.2075665 C73.3779378,13.7583581 75.2720906,16.0623569 76.6013406,19.1196319 L77.4985793,21.1734305 L78.262894,23.5252028 L78.2628948,39.6564417 C78.2628948,42.0934001 78.0081257,44.4195527 77.4985799,46.6349693 C76.989034,48.850386 76.1471883,50.7999234 74.9730175,52.4836401 C73.7988467,54.1673567 72.2480783,55.5076637 70.3206658,56.5046012 C68.3932533,57.5015387 66.0117161,58 63.1759827,58 C60.2959411,58 57.7260963,57.5015387 55.4663713,56.5046012 C53.2066463,55.5076637 51.2792627,54.1008952 49.6841627,52.2842536 L50.6810952,51.2873211 C52.3648119,53.1925794 54.2811185,54.577194 56.4300727,55.4412065 C58.5790269,56.305219 60.8276411,56.7372188 63.1759827,56.7372188 C65.8344827,56.7372188 68.0498662,56.2498345 69.8221995,55.2750511 C71.5945328,54.3002678 72.9902244,53.0153454 74.0093161,51.4202454 C75.0284077,49.8251454 75.7484073,47.9974541 76.1693365,45.9371166 C76.5902657,43.8767791 76.8007271,41.738934 76.8007271,39.5235174 L76.8007271,33.1431493 Z"
                    (Color.rgb 248 231 28)
                    (Color.rgb 80 227 194)
                    80
                    60
                ]
            ]
        ]


hamburger : Signal.Address Action -> Bool -> Html
hamburger address open =
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
                    , "top" => "35px"
                    , "right" => "35px"
                    , "cursor" => "pointer"
                    , "zIndex" => "998"
                    ]
                , onClick address ToggleMenu
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


title' : Html
title' =
    h1
        [ style
            [ "position" => "relative"
            , "top" => "41px"
            , "textAlign" => "center"
            ]
        ]
        [ text "PROJECTS" ]


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
                    , "backgroundColor"
                        => if hovered == (Just category) then
                            "#ffffff"
                           else
                            "inherit"
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
    { title : String
    , subtitle : String
    , date : String
    , category : Category
    , w : Float
    , h : Float
    }


convertPartialProject : Menu.PartialProject -> Maybe Project
convertPartialProject project =
    let
        andThen = Maybe.andThen

        readCategory =
            case project.category of
                "Interface" ->
                    Just Interface

                "UX" ->
                    Just UX

                "Graphic" ->
                    Just Graphic

                "Photograph" ->
                    Just Photograph

                _ ->
                    Nothing
    in
        project.title
            `andThen` \title ->
                        project.subtitle
                            `andThen` \subtitle ->
                                        project.date
                                            `andThen` \date ->
                                                        readCategory
                                                            `andThen` \category ->
                                                                        project.w
                                                                            `andThen` \w ->
                                                                                        project.h
                                                                                            `andThen` \h ->
                                                                                                        (Just <| Project title subtitle date category w h)


projects : Signal.Address Action -> ( Category, List ( ID, Project ) ) -> Html
projects address ( current, projectList ) =
    div
        [ style
            [ "position" => "relative"
            ]
        ]
        <| List.map
            (viewProject address)
        <| List.filter
            (\( _, project ) -> project.category == current || current == All)
            projectList


viewProject : Signal.Address Action -> ( ID, Project ) -> Html
viewProject address ( id, project ) =
    div
        [ style
            [ "width" => (toString project.w ++ "px")
            , "height" => (toString project.h ++ "px")
            , "backgroundColor" => "#ffffff"
            , "color" => "#333333"
            , "fontFamily" => "monospace"
            , "position" => "relative"
            , "left" => "40px"
            , "display" => "inline-block"
            , "margin" => "11px"
            ]
        , onClick address (RemoveProject id)
        ]
        [ div
            [ style
                [ "position" => "absolute"
                , "bottom" => "20px"
                , "left" => "40px"
                ]
            ]
            [ h3 [] [ text <| "<" ++ project.title ++ ">" ]
            , h4 [] [ text <| project.subtitle ++ " - " ++ project.date ]
            ]
        ]
