module List.Reach exposing (element, elementEach, elementIndexEach)

{-| Reach for `List`s.

@docs element, elementEach, elementIndexEach

-}

import Linear exposing (DirectionLinear, ExpectedIndexInRange(..))
import List.Linear
import Reach


{-| Reach all the elements contained inside a `List`

    import Reach exposing (each, view, map)
    import Record

    listRecord : { foo : List { bar : Int } }
    listRecord =
        { foo =
            [ { bar = 2 }
            , { bar = 3 }
            , { bar = 4 }
            ]
        }

    view (Record.foo << each << Record.bar) listRecord
    --> [2, 3, 4]

    map (Record.foo << each << Record.bar) ((+) 1) listRecord
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


{-| Reach each element contained inside a `List` including the index of each element

    import Reach exposing (view, mapOver)
    import List.Reach as List
    import Tuple.Reach as Tuple
    import Record

    listRecord : { foo : List { bar : Int } }
    listRecord =
        { foo =
            [ { bar = 2 }
            , { bar = 3 }
            , { bar = 4 }
            ]
        }

    listRecord |> Reach.view (Record.foo << List.elementIndexEach)
    --> [ ( 0, { bar = 2 } ), ( 1, { bar = 3 } ), ( 2, { bar = 4 } ) ]

    listRecord
        |> Reach.mapOver
            (Record.foo << List.elementIndexEach)
            (\{ index, element } ->
                case index of
                    0 ->
                        element

                    _ ->
                        { bar = element.bar * 10 }
            )
    --> { foo = [ { bar = 2 }, { bar = 30 }, { bar = 40 } ] }

    listRecord
        |> Reach.view (Record.foo << List.elementIndexEach << Record.element << Record.bar)
    --> [ 2, 3, 4 ]

    listRecord
        |> Reach.mapOver
            (Record.foo << List.elementIndexEach << Record.element << Record.bar)
            ((+) 1)
    --> { foo = [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
elementIndexEach :
    Reach.Elements
        (List element)
        { element : element, index : Int }
        (List elementView)
        elementView
        (List elementMapped)
        { element : elementMapped, index : Int }
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
                        { element = element_, index = index } |> elementIndexMap |> .element
                    )
        }


{-| Focus a `List` element at a given index in a [direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import Reach exposing (view)
    import List.Reach as List
    import Record

    bars : List { bar : String }
    bars =
        [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars |> Reach.view (List.element 1)
    --> Just { bar = "Things" }

    bars |> Reach.view (List.element 9000)
    --> Nothing

    bars |> Reach.view (List.element 0 << Record.bar)
    --> Just "Stuff"

    bars |> Reach.mapOver (List.element 0 << Record.bar) (\_ -> "Whatever")
    --> [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars |> Reach.mapOver (List.element 9000 << Record.bar) (\_ -> "Whatever")
    --> bars

-}
element :
    ( DirectionLinear, Int )
    ->
        Reach.Maybe
            (List element)
            element
            elementView
            (List element)
            element
element focusLocation =
    Reach.maybe ("element " ++ (focusLocation |> Linear.locationToString))
        { access =
            \list ->
                case list |> List.Linear.element focusLocation of
                    Err (ExpectedIndexForLength _) ->
                        Nothing

                    Ok elementFound ->
                        elementFound |> Just
        , map =
            \elementMap ->
                List.Linear.elementAlter ( focusLocation, elementMap )
        }
