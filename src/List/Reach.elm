module List.Reach exposing (element, elementEach)

{-| Reach into a `List`

@docs element, elementEach

-}

import List.Extra as List
import Reach


{-| Reach each element contained inside a `List`

    import Reach
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
        |> Reach.view (Record.foo |> Reach.into List.Reach.elementEach |> Reach.into Record.bar)
    --> [2, 3, 4]

    listRecord
        |> Reach.mapOver
            (Record.foo |> Reach.into List.Reach.elementEach |> Reach.into Record.bar)
            (\n -> n + 1)
    --> { foo = [ { bar = 3 }, { bar = 4}, { bar = 5 } ] }

-}
elementEach :
    Reach.Elements
        (List element)
        element
        (List elementView)
        elementView
        (List elementMapped)
        elementMapped
elementEach =
    Reach.elements "element each"
        { view = List.map
        , map = List.map
        }


{-| Reach a `List`'s element at a given index

    import Reach
    import List.Reach
    import Record

    bars : List { bar : String }
    bars =
        [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars |> Reach.view (List.Reach.element 1)
    --> Just { bar = "Things" }

    bars |> Reach.view (List.Reach.element 9000)
    --> Nothing

    bars |> Reach.view (List.Reach.element 0 |> Reach.into Record.bar)
    --> Just "Stuff"

    bars
        |> Reach.mapOver
            (List.Reach.element 0 |> Reach.into Record.bar)
            (\_ -> "Whatever")
    --> [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars
        |> Reach.mapOver
            (List.Reach.element 9000 |> Reach.into Record.bar)
            (\_ -> "Whatever")
    --> bars

-}
element :
    Int
    ->
        Reach.Maybe
            (List element)
            element
            elementView
            (List element)
            element
element index =
    Reach.maybe
        ("element "
            ++ (index |> String.fromInt)
        )
        { access =
            \list ->
                list |> List.getAt index
        , map =
            \elementMap ->
                List.updateAt index elementMap
        }
