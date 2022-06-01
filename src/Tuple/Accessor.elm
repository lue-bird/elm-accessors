module Tuple.Accessor exposing (first, second)

{-| Accessors for `( , )` tuples.

@docs first, second

-}

import Accessor exposing (Relation, for1To1)


{-| Lens over the first component of a Tuple.

    import Accessors exposing (access, map)
    import Tuple.Accessor as Tuple

    charging : ( String, Int )
    charging =
        ( "It's over", 1 )

    charging |> access Tuple.first
    --> "It's over"

    charging |> map Tuple.first (\_ -> "It's map")
    --> ( "It's over", 1 )

    charging
        |> map Tuple.first (\m -> (m |> String.toUpper) ++ "!!!")
    --> ( "IT'S OVER!!!", 1 )

-}
first : Relation sub reachable wrap -> Relation ( sub, x ) reachable wrap
first =
    for1To1
        { description = { structure = "Tuple", focus = "first" }
        , access = Tuple.first
        , map = Tuple.mapFirst
        }


{-| Lens map the second component of a Tuple.

    import Accessors exposing (access, map)
    import Tuple.Accessor as Tuple

    jo : ( String, Int )
    jo =
        ( "Hi there", 1 )

    jo |> access Tuple.second
    --> 1

    jo |> map Tuple.second (\_ -> 1125)
    --> ( "Hi there", 1125 )

    jo
        |> map Tuple.second (\_ -> 1125)
        |> map Tuple.first (\m -> (m |> String.toUpper) ++ "!!!")
        |> map Tuple.second ((*) 8)
    --> ( "HI THERE!!!", 9000 )

-}
second : Relation sub reachable wrap -> Relation ( x, sub ) reachable wrap
second =
    for1To1
        { description = { structure = "Tuple", focus = "second" }
        , access = Tuple.second
        , map = Tuple.mapSecond
        }
