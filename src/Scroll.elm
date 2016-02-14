module Scroll (offset, xOffset, yOffset) where

{-| Scrolling support for pages that need scrollbar awareness

# Scroll
@docs offset, xOffset, yOffset
-}

import Native.Scroll


{-| Current scroll status. -}
offset : Signal ( Int, Int )
offset =
    Native.Scroll.offset


{-| Current scroll status on the X axis. -}
xOffset : Signal Int
xOffset =
    Signal.map fst offset


{-| Current scroll status on the Y axis. -}
yOffset : Signal Int
yOffset =
    Signal.map snd offset
