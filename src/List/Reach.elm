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
        |> Reach.view (Record.foo << List.Reach.elementEach << Record.bar)
    --> [2, 3, 4]

    listRecord
        |> Reach.mapOver
            (Record.foo << List.Reach.elementEach << Record.bar)
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


{-| Reach each element contained inside a `List` including the index of each element.

Both examples ↓ show that this is always the final step
before using the created reach to [map](Reach#mapOver) or [`view`](Reach#view) inside a structure

    import Reach
    import List.Reach
    import Record

    listRecord : { foo : List { bar : Int } }
    listRecord =
        { foo =
            [ { bar = 2 }
            , { bar = 3 }
            , { bar = 4 }
            ]
        }

    listRecord |> Reach.view (Record.foo << List.Reach.elementIndexEach)
    --> [ { index = 0, element = { bar = 2 } }
    --> , { index = 1, element = { bar = 3 } }
    --> , { index = 2, element = { bar = 4 } }
    --> ]

    listRecord
        |> Reach.mapOver
            (Record.foo << List.Reach.elementIndexEach)
            (\{ index, element } ->
                case index of
                    0 ->
                        element
                    _ ->
                        { bar = element.bar * 10 }
            )
    --> { foo = [ { bar = 2 }, { bar = 30 }, { bar = 40 } ] }

-}
elementIndexEach :
    Reach.Elements
        (List element)
        { element : element, index : Int }
        (List elementView)
        elementView
        (List elementMapped)
        elementMapped
elementIndexEach =
    Reach.elements "{element,index} each"
        { view =
            \elementIndexFocusView ->
                List.indexedMap
                    (\index element_ ->
                        { element = element_, index = index } |> elementIndexFocusView
                    )
        , map =
            \elementIndexMap ->
                List.indexedMap
                    (\index element_ ->
                        { element = element_, index = index } |> elementIndexMap
                    )
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

    bars |> Reach.view (List.Reach.element 0 << Record.bar)
    --> Just "Stuff"

    bars
        |> Reach.mapOver
            (List.Reach.element 0 << Record.bar)
            (\_ -> "Whatever")
    --> [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars
        |> Reach.mapOver
            (List.Reach.element 9000 << Record.bar)
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
