module Tuple.Map exposing (first, second)

{-| map tuple parts `( first, second )`

@docs first, second

-}

import Map exposing (Map)
import Tuple


{-| Lens over the first component of a Tuple.

    import Map
    import Tuple.Map


    ( "It's over", 1 ) |> Map.view Tuple.Map.first
    --> "It's over"

    ( "It's over", 1 )
        |> Map.over Tuple.Map.first (\_ -> "It's over")
    --> ( "It's over", 1 )

    ( "It's over", 1 )
        |> Map.over
            Tuple.Map.first
            (\m -> (m |> String.toUpper) ++ "!!!")
    --> ( "IT'S OVER!!!", 1 )

-}
first :
    Map
        ( first, second )
        first
        ( firstMapped, second )
        firstMapped
first =
    Map.at "first" Tuple.mapFirst


{-| Lens map the second component of a Tuple.

    import Map
    import Tuple.Map as Tuple

    jo : ( String, Int )
    jo =
        ( "Hi there", 1 )

    jo |> Map.over Tuple.Map.second (\_ -> 1125)
    --> ( "Hi there", 1125 )

    jo
        |> Map.over Tuple.Map.second (\_ -> 1125)
        |> Map.over
            Tuple.Map.first
            (\m -> (m |> String.toUpper) ++ "!!!")
        |> Map.over Tuple.Map.second (\n -> n * 8)
    --> ( "HI THERE!!!", 9000 )

-}
second :
    Map
        ( first, second )
        second
        ( first, secondMapped )
        secondMapped
second =
    Map.at "second" Tuple.mapSecond
