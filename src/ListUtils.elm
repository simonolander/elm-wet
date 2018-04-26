module ListUtils exposing (..)


zip : List a -> List b -> List (a, b)
zip list1 list2 =
    case (list1, list2) of
        ((h1::t1), (h2::t2)) ->
            (h1, h2) :: (zip t1 t2)
        _ -> []


unzip3 : List (a, b, c) -> (List a, List b, List c)
unzip3 list =
    case list of
        (a, b, c) :: tail ->
            let
                (l1, l2, l3) = unzip3 tail
            in
                (a :: l1, b :: l2, c :: l3)
        _ -> ([], [], [])
