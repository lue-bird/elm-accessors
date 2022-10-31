module Array.Map exposing (each, element)

{-| [`Map`](Map#Map) an `Array`

@docs each, element

-}

import Array exposing (Array)
import Map exposing (Alter, Map)


{-| Map each element contained inside an `Array`

    import Array exposing (Array)
    import Map
    import Array.Map
    import Record

    effect : { trail : Array { sparkle : Int } }
    effect =
        { trail =
            Array.fromList [ { sparkle = 2 }, { sparkle = 3 }, { sparkle = 4 } ]
        }

    effect
        |> Map.over
            (Record.trail << Array.Map.each << Record.sparkle)
            (\n -> n + 1)
    --> { trail =
    -->     Array.fromList [ { sparkle = 3 }, { sparkle = 4 }, { sparkle = 5 } ]
    --> }

-}
each : Map (Array element) element (Array elementMapped) elementMapped
each =
    Map.at "each" Array.map


{-| Focus an `Array` element at a given index in a [direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)

    import Array exposing (Array)
    import Map
    import Array.Map
    import Record

    tags : Array { tag : String }
    tags =
        Array.fromList [ { tag = "Stuff" }, { tag =  "Things" }, { tag = "Woot" } ]

    tags
        |> Map.over (Array.Map.element 0 << Record.tag) (\_ -> "Whatever")
    --> Array.fromList
    -->     [ { tag = "Whatever" }, { tag =  "Things" }, { tag = "Woot" } ]

    tags
        |> Map.over
            (Array.Map.element 9000 << Record.tag)
            (\_ -> "Whatever")
    --> tags

-}
element : Int -> Alter (Array elementMapped) elementMapped
element index =
    Map.at (index |> String.fromInt)
        (\alter array ->
            case array |> Array.get index of
                Nothing ->
                    array

                Just elementAtIndex ->
                    array |> Array.set index (elementAtIndex |> alter)
        )
