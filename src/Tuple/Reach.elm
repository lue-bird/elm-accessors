module Tuple.Reach exposing (first, second)

{-| Reach `( , )` tuple parts.

@docs first, second

-}

import Reach


{-| Lens over the first component of a Tuple.

    import Reach exposing (view, mapOver)
    import Tuple.Reach as Tuple

    charging : ( String, Int )
    charging =
        ( "It's over", 1 )

    charging |> Reach.view Tuple.first
    --> "It's over"

    charging |> Reach.mapOver Tuple.first (\_ -> "It's map")
    --> ( "It's over", 1 )

    charging
        |> Reach.mapOver Tuple.first (\m -> (m |> String.toUpper) ++ "!!!")
    --> ( "IT'S OVER!!!", 1 )

-}
first :
    Reach.Part
        ( first, second )
        first
        reachView
        ( firstMapped, second )
        firstMapped
first =
    Reach.part "first"
        { access = Tuple.first
        , map = Tuple.mapFirst
        }


{-| Lens map the second component of a Tuple.

    import Reach exposing (view, mapOver)
    import Tuple.Reach as Tuple

    jo : ( String, Int )
    jo =
        ( "Hi there", 1 )

    jo |> Reach.view Tuple.second
    --> 1

    jo |> Reach.mapOver Tuple.second (\_ -> 1125)
    --> ( "Hi there", 1125 )

    jo
        |> Reach.mapOver Tuple.second (\_ -> 1125)
        |> Reach.mapOver Tuple.first (\m -> (m |> String.toUpper) ++ "!!!")
        |> Reach.mapOver Tuple.second ((*) 8)
    --> ( "HI THERE!!!", 9000 )

-}
second :
    Reach.Part
        ( first, second )
        second
        reachView
        ( first, secondMapped )
        secondMapped
second =
    Reach.part "second"
        { access = Tuple.second
        , map = Tuple.mapSecond
        }
