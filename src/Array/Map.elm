module Array.Map exposing (each, element)

{-| map into an `Array`

@docs each, element

-}

import Array exposing (Array)
import Map exposing (Alter, Map)


{-| Map each element contained inside an `Array`

    import Array exposing (Array)
    import Map
    import Array.Map
    import Record

    fooBarray : { foo : Array { bar : Int } }
    fooBarray =
        { foo =
            Array.fromList [ { bar = 2 }, { bar = 3 }, { bar = 4 } ]
        }

    fooBarray
        |> Map.over
            (Record.foo << Array.Map.each << Record.bar)
            (\n -> n + 1)
    --> { foo = Array.fromList [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
each : Map (Array element) element (Array elementMapped) elementMapped
each =
    Map.at "each" Array.map


{-| Focus an `Array` element at a given index in a [direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)

    import Array exposing (Array)
    import Map
    import Array.Map
    import Record

    barray : Array { bar : String }
    barray =
        Array.fromList [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray
        |> Map.over (Array.Map.element 0 << Record.bar) (\_ -> "Whatever")
    --> Array.fromList
    -->     [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray
        |> Map.over
            (Array.Map.element 9000 << Record.bar)
            (\_ -> "Whatever")
    --> barray

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
