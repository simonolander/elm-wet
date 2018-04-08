module RandomExtra exposing (..)

import Random exposing (Generator, int, map2)

{-| NoRedInc, http://package.elm-lang.org/packages/NoRedInk/elm-random-extra/2.1.1/Random-Extra
-}
docs : String
docs = "NoRedInc, http://package.elm-lang.org/packages/NoRedInk/elm-random-extra/2.1.1/Random-Extra"


{-| Create a generator that always returns the same value.
-}
constant : a -> Generator a
constant value =
  Random.map (\_ -> value) Random.bool


flattenList : List (Generator a) -> Generator (List a)
flattenList generators =
  case generators of
      [] -> constant []
      g :: gs -> map2 (::) g (flattenList gs)


{-| Generator that randomly selects an element from a list.
-}
select : List a -> Generator (Maybe a)
select list =
  Random.map (\index -> get index list)
             (int 0 (List.length list - 1))


get : Int -> List a -> Maybe a
get index list =
  if index < 0
  then Nothing
  else
    case List.drop index list of
      [] -> Nothing
      x :: xs -> Just x
