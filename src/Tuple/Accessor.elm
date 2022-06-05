module Tuple.Accessor exposing (first, second)

{-| Accessors for `( , )` tuples.

@docs first, second

-}

import Accessor exposing (Lens, lens)


{-| Lens over the first component of a Tuple.

    import Accessor exposing (view, mapOver)
    import Tuple.Accessor

    charging : ( String, Int )
    charging =
        ( "It's over", 1 )

    charging |> view Tuple.Accessor.first
    --> "It's over"

    charging |> mapOver Tuple.Accessor.first (\_ -> "Hope")
    --> ( "Hope", 1 )

    charging
        |> mapOver Tuple.Accessor.first (\m -> (m |> String.toUpper) ++ "!!!")
    --> ( "IT'S OVER!!!", 1 )

-}
first : Lens ( partFirst, partSecond ) partFirst partFirstFocus partFirstFocusView
first =
    lens
        { description = "first"
        , view = Tuple.first
        , map = Tuple.mapFirst
        }


{-| Lens map the second component of a Tuple.

    import Accessor exposing (view, mapOver)
    import Tuple.Accessor

    jo : ( String, Int )
    jo =
        ( "Hi there", 1 )

    jo |> view Tuple.Accessor.second
    --> 1

    jo |> mapOver Tuple.Accessor.second (\_ -> 1125)
    --> ( "Hi there", 1125 )

    jo
        |> mapOver Tuple.Accessor.second (\_ -> 1125)
        |> mapOver Tuple.Accessor.first (\m -> (m |> String.toUpper) ++ "!!!")
        |> mapOver Tuple.Accessor.second ((*) 8)
    --> ( "HI THERE!!!", 9000 )

-}
second : Lens ( partFirst, partSecond ) partSecond partSecondFocus partSecondFocusView
second =
    lens
        { description = "second"
        , view = Tuple.second
        , map = Tuple.mapSecond
        }
