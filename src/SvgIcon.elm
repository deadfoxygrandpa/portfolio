module SvgIcon exposing (icon, icon2)

import Color exposing (Color)
import Svg exposing (Svg)
import Svg.Attributes


{- Copied from https://github.com/elm-community/elm-material-icons -}


icon : String -> Color -> Int -> Int -> Svg
icon path color w h =
    let
        stringWidth =
            toString w

        stringHeight =
            toString h

        stringColor =
            toRgbaString color
    in
        Svg.svg
            [ Svg.Attributes.width stringWidth
            , Svg.Attributes.height stringHeight
            , Svg.Attributes.viewBox "0 0 80 60"
            ]
            [ Svg.path
                [ Svg.Attributes.d path
                , Svg.Attributes.fill stringColor
                ]
                []
            ]


icon2 : String -> String -> Color -> Color -> Int -> Int -> Svg
icon2 path1 path2 color1 color2 w h =
    let
        stringWidth =
            toString w

        stringHeight =
            toString h

        stringColor1 =
            toRgbaString color1

        stringColor2 =
            toRgbaString color2
    in
        Svg.svg
            [ Svg.Attributes.width stringWidth
            , Svg.Attributes.height stringHeight
            , Svg.Attributes.viewBox "0 0 80 60"
            ]
            [ Svg.path
                [ Svg.Attributes.d path1
                , Svg.Attributes.fill stringColor1
                ]
                []
            , Svg.path
                [ Svg.Attributes.d path2
                , Svg.Attributes.fill stringColor2
                ]
                []
            ]


toRgbaString : Color -> String
toRgbaString color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        "rgba("
            ++ toString red
            ++ ","
            ++ toString green
            ++ ","
            ++ toString blue
            ++ ","
            ++ toString alpha
            ++ ")"
