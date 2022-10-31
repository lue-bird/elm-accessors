module List.Map exposing (element, each)

{-| [`Map`](Map#Map) a `List`

@docs element, each

-}

import List.Extra as List
import Map exposing (Alter, Map)


{-| Map each element contained inside a `List`

    import Map
    import Record

    effect : { trail : List { sparkle : Int } }
    effect =
        { trail =
            [ { sparkle = 2 }
            , { sparkle = 3 }
            , { sparkle = 4 }
            ]
        }

    effect
        |> Map.over
            (Record.trail << List.Map.each << Record.sparkle)
            (\n -> n + 1)
    --> { trail = [ { sparkle = 3 }, { sparkle = 4}, { sparkle = 5 } ] }

-}
each : Map (List element) element (List elementMapped) elementMapped
each =
    Map.at "each" List.map


{-| Map a `List`'s element at a given index

    import Map
    import List.Map
    import Record

    sparkles : List { sparkle : String }
    sparkles =
        [ { sparkle = "Stuff" }, { sparkle =  "Things" }, { sparkle = "Woot" } ]

    sparkles
        |> Map.over
            (List.Map.element 0 << Record.sparkle)
            (\_ -> "Whatever")
    --> [ { sparkle = "Whatever" }, { sparkle =  "Things" }, { sparkle = "Woot" } ]

    sparkles
        |> Map.over
            (List.Map.element 9000 << Record.sparkle)
            (\_ -> "Whatever")
    --> sparkles

-}
element : Int -> Alter (List element) element
element index =
    Map.at (index |> String.fromInt)
        (\elementAlter list -> list |> List.updateAt index elementAlter)
