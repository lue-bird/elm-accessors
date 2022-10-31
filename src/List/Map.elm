module List.Map exposing (element, each)

{-| map into a `List`

@docs element, each

-}

import List.Extra as List
import Map exposing (Alter, Map)


{-| Map each element contained inside a `List`

    import Map
    import Record

    listRecord : { foo : List { bar : Int } }
    listRecord =
        { foo =
            [ { bar = 2 }
            , { bar = 3 }
            , { bar = 4 }
            ]
        }

    listRecord
        |> Map.over
            (Record.foo << List.Map.each << Record.bar)
            (\n -> n + 1)
    --> { foo = [ { bar = 3 }, { bar = 4}, { bar = 5 } ] }

-}
each : Map (List element) element (List elementMapped) elementMapped
each =
    Map.at "each" List.map


{-| Map a `List`'s element at a given index

    import Map
    import List.Map
    import Record

    bars : List { bar : String }
    bars =
        [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars
        |> Map.over
            (List.Map.element 0 << Record.bar)
            (\_ -> "Whatever")
    --> [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars
        |> Map.over
            (List.Map.element 9000 << Record.bar)
            (\_ -> "Whatever")
    --> bars

-}
element : Int -> Alter (List element) element
element index =
    Map.at (index |> String.fromInt)
        (\elementAlter list -> list |> List.updateAt index elementAlter)
