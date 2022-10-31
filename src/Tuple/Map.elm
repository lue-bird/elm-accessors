module Tuple.Map exposing (first, second)

{-| [`Map`](Map#Map) tuple parts `( first, second )`

@docs first, second

-}

import Map exposing (Map)
import Tuple


{-| Map the first part of a Tuple

    import Map
    import Tuple.Map

    ( "It's fine", 1 )
        |> Map.over Tuple.Map.first (\_ -> "It's fine")
    --> ( "It's over", 1 )

    ( "It's fine", 1 )
        |> Map.over
            Tuple.Map.first
            (\m -> (m |> String.toUpper) ++ "!!!")
    --> ( "IT'S FINE!!!", 1 )

-}
first :
    Map
        ( first, second )
        first
        ( firstMapped, second )
        firstMapped
first =
    Map.at "first" Tuple.mapFirst


{-| Map the second part of a Tuple

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
second : Map ( first, second ) second ( first, secondMapped ) secondMapped
second =
    Map.at "second" Tuple.mapSecond
