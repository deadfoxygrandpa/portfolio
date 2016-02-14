module ColorScheme (..) where

import Color
import Random


type alias Color =
  { color : Color.Color, hex : String }


highlight1 : Color
highlight1 =
  Color (Color.rgb 80 227 194) "#50e3c2"


highlight2 : Color
highlight2 =
  Color (Color.rgb 248 231 28) "#f8e71c"


background : Color
background =
  Color (Color.rgb 242 249 250) "#f2f9fa"


accent1 : Color
accent1 =
  Color (Color.rgb 184 233 134) "#b8e986"


accent2 : Color
accent2 =
  Color (Color.rgb 245 166 35) "#f5a623"


accent3 : Color
accent3 =
  Color (Color.rgb 74 144 226) "#4a90e2"


accent4 : Color
accent4 =
  Color (Color.rgb 155 155 155) "#9b9b9b"


randomColor : Random.Generator Color
randomColor =
  let
    choose n =
      if n == 0 then
        accent1
      else if n == 1 then
        accent2
      else if n == 2 then
        accent3
      else if n == 3 then
        accent4
      else if n == 4 then
        highlight1
      else
        highlight2
  in
    Random.map choose (Random.int 0 5)
