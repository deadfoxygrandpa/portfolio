module Scroll (offset, xOffset, yOffset) where

import Native.Scroll


offset : Signal ( Int, Int )
offset =
    Native.Scroll.offset


xOffset : Signal Int
xOffset =
    Signal.map fst offset


yOffset : Signal Int
yOffset =
    Signal.map snd offset
