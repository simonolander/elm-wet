module SquareText exposing (..)

import Collage
import Color


height : Float
height = 1


width : Float
width = height * 3 / 5


thicc : Float
thicc = height / 5


small : Float
small = thicc / 5


x0 : Float
x0 = -width / 2


x1 : Float
x1 = x0 + thicc


x2 : Float
x2 = x1 + thicc


x3 : Float
x3 = x2 + thicc


y0 : Float
y0 = -height / 2


y1 : Float
y1 = y0 + thicc


y2 : Float
y2 = y1 + thicc


y3 : Float
y3 = y2 + thicc


y4 : Float
y4 = y3 + thicc


y5 : Float
y5 = y4 + thicc



text : String -> (Collage.Form, Float, Float)
text string =
    let
        length = toFloat (String.length string)
        totalWidth = length * width + (length - 1) * thicc
        move index shape =
            Collage.moveX (-totalWidth / 2 + width / 2 + (width + thicc) * (toFloat index)) shape

        form =
            string
                |> String.toList
                |> List.map toCharacter
                |> List.map (Collage.filled Color.yellow)
                |> List.indexedMap move
                |> Collage.group

    in
        (form, totalWidth, height)


{-
       0 1 2 3
     5 ┏━┳━┳━┓ 5
     4 ┣━╋━╋━┫ 4
     3 ┣━╋━╋━┫ 3
     2 ┣━╋━╋━┫ 2
     1 ┣━╋━╋━┫ 1
     0 ┗━┻━┻━┛ 0
       0 1 2 3
-}
toCharacter : Char -> Collage.Shape
toCharacter char =
    case char of
        '0' ->
            Collage.polygon
                [ (x0, y0)
                , (x3, y0)
                , (x3, y4 - small)
                , (x2, y4 - small)
                , (x2, y1)
                , (x1, y1)
                , (x1, y4)
                , (x2, y4)
                , (x3, y5)
                , (x0, y5)
                ]
        '1' ->
            Collage.polygon
                [ (x1, y0)
                , (x2, y0)
                , (x2, y5)
                , (x1, y5)
                , (x1, y4)
                , (x0, y4)
                , (x0, y3)
                , (x1, y3)
                ]
        '2' ->
            Collage.polygon
                [ (x0, y0)
                , (x3, y0)
                , (x3, y1)
                , (x1, y1)
                , (x1, y2)
                , (x3, y2)
                , (x3, y5)
                , (x0, y5)
                , (x0, y4)
                , (x2, y4)
                , (x2, y3)
                , (x0, y3)
                ]
        '3' ->
            Collage.polygon
                [ (x0, y0)
                , (x3, y0)
                , (x3, y5)
                , (x0, y5)
                , (x0, y4)
                , (x2, y4)
                , (x2, y3)
                , (x0, y3)
                , (x0, y2)
                , (x2, y2)
                , (x2, y1)
                , (x0, y1)
                ]
        '4' ->
            Collage.polygon
                [ (x2, y0)
                , (x3, y0)
                , (x3, y5)
                , (x2, y5)
                , (x2, y3)
                , (x1, y3)
                , (x1, y5)
                , (x0, y5)
                , (x0, y2)
                , (x2, y2)
                ]
        '5' ->
            Collage.polygon
                [ (x0, y0)
                , (x3, y0)
                , (x3, y3)
                , (x1, y3)
                , (x1, y4)
                , (x3, y4)
                , (x3, y5)
                , (x0, y5)
                , (x0, y2)
                , (x2, y2)
                , (x2, y1)
                , (x1, y1)
                ]
        '6' ->
            Collage.polygon
                [ (x0, y0)
                , (x3, y0)
                , (x3, y3)
                , (x1, y3)
                , (x1, y4)
                , (x3, y4)
                , (x3, y5)
                , (x0, y5)
                , (x0, y1 + small)
                , (x1, y1 + small)
                , (x1, y2)
                , (x2, y2)
                , (x2, y1)
                , (x1, y1)
                ]
        '7' ->
            Collage.polygon
                [ (x3, y0)
                , (x3, y5)
                , (x0, y5)
                , (x0, y4)
                , (x2, y4)
                , (x2, y0)
                ]
        '8' ->
            Collage.polygon
                [ (x0, y0)
                , (x3, y0)
                , (x3, y2 - small)
                , (x2, y2 - small)
                , (x2, y1)
                , (x1, y1)
                , (x1, y2)
                , (x2, y2)
                , (x3, y3)
                , (x3, y4 - small)
                , (x2, y4 - small)
                , (x2, y3)
                , (x1, y3)
                , (x1, y4)
                , (x2, y4)
                , (x3, y5)
                , (x0, y5)
                ]
        '9' ->
            Collage.polygon
                [ (x0, y0)
                , (x3, y0)
                , (x3, y4 - small)
                , (x2, y4 - small)
                , (x2, y3)
                , (x1, y3)
                , (x1, y4)
                , (x2, y4)
                , (x3, y5)
                , (x0, y5)
                , (x0, y2)
                , (x2, y2)
                , (x2, y1)
                , (x0, y1)
                ]
        _ ->
            Collage.polygon
                [ (x0, y0)
                , (x3, y0)
                , (x3, y5)
                , (x0, y5)
                ]
