module Tuple.Reach exposing (first, second)

{-| Reach tuple parts `( first, second )`

@docs first, second

-}

import Reach


{-| Lens over the first component of a Tuple.

    import Reach
    import Tuple.Reach


    ( "It's over", 1 ) |> Reach.view Tuple.Reach.first
    --> "It's over"

    ( "It's over", 1 )
        |> Reach.mapOver Tuple.Reach.first (\_ -> "It's mapOver")
    --> ( "It's mapOver", 1 )

    ( "It's over", 1 )
        |> Reach.mapOver
            Tuple.Reach.first
            (\m -> (m |> String.toUpper) ++ "!!!")
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

    import Reach
    import Tuple.Reach as Tuple

    jo : ( String, Int )
    jo =
        ( "Hi there", 1 )

    jo |> Reach.view Tuple.Reach.second
    --> 1

    jo |> Reach.mapOver Tuple.Reach.second (\_ -> 1125)
    --> ( "Hi there", 1125 )

    jo
        |> Reach.mapOver Tuple.Reach.second (\_ -> 1125)
        |> Reach.mapOver
            Tuple.Reach.first
            (\m -> (m |> String.toUpper) ++ "!!!")
        |> Reach.mapOver Tuple.Reach.second (\n -> n * 8)
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
