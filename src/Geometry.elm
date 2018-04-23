module Geometry exposing (..)

import Collage


type alias Point = (Float, Float)

type alias Rectangle =
    { x: Float
    , y: Float
    , width: Float
    , height: Float
    }


rectangleContainsPoint : Rectangle -> Point -> Bool
rectangleContainsPoint r (px, py) =
    r.x <= px && px <= r.x + r.width && r.y <= py && py <= r.y + r.height


rectanglesIntersect : Rectangle -> Rectangle -> Bool
rectanglesIntersect r1 r2 =
    not (  r1.x + r1.width < r2.x
        || r2.x + r2.width < r1.x
        || r1.y + r1.height < r2.y
        || r2.y + r2.height < r1.y
        )


rectanglePoints : Rectangle -> List Point
rectanglePoints { x, y, width, height } =
    [ (x, y)
    , (x + width, y)
    , (x + width, y + height)
    , (x, y + height)
    ]


mergeRectangles : Rectangle -> Rectangle -> Rectangle
mergeRectangles r1 r2 =
    let
        left = min r1.x r2.x
        top = max (r1.y + r1.height) (r2.y + r2.height)
        right = max (r1.x + r1.width) (r2.x + r2.width)
        bottom = min r1.y r2.y
    in
        { x = left
        , y = bottom
        , width = right - left
        , height = bottom - top
        }


moveY : Float -> Rectangle -> Rectangle
moveY dy r =
    { r | y = r.y + dy }
