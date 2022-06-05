module List.Accessor exposing (element, elementEach, elementIndexEach)

{-| Accessors for `List`s.

@docs element, elementEach, elementIndexEach

-}

import Accessor exposing (Lens, Optional, Traversal, lens, onJust, traversal)
import Linear exposing (DirectionLinear, ExpectedIndexInRange(..))
import Linear.Extra as Linear
import List.Linear


{-| This accessor combinator lets you view values inside List.

    import Accessors exposing (each, view, map)
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
    Traversal
        (List element)
        element
        (List elementFocusView)
        elementFocus
        elementFocusView
elementEach =
    traversal
        { name = "element each"
        , get = List.map
        , over = List.map
        }


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (view, over)
    import List.Accessor as List
    import Tuple.Accessor as Tuple
    import Record

    listRecord : { foo : List { bar : Int } }
    listRecord =
        { foo =
            [ { bar = 2 }
            , { bar = 3 }
            , { bar = 4 }
            ]
        }

    listRecord |> view (Record.foo << List.elementIndexEach)
    --> [ ( 0, { bar = 2 } ), ( 1, { bar = 3 } ), ( 2, { bar = 4 } ) ]

    listRecord
        |> over
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
        |> view (Record.foo << List.elementIndexEach << Record.element << Record.bar)
    --> [ 2, 3, 4 ]

    listRecord
        |> over
            (Record.foo << List.elementIndexEach << Record.element << Record.bar)
            ((+) 1)
    --> { foo = [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
elementIndexEach :
    Traversal
        (List element)
        { index : Int, element : element }
        (List elementView)
        elementFocus
        elementView
elementIndexEach =
    traversal
        { name = "{element,index} each"
        , get =
            \elementAlter ->
                List.indexedMap
                    (\index element_ ->
                        { element = element_, index = index } |> elementAlter
                    )
        , over =
            \elementAlter ->
                List.indexedMap
                    (\index element_ ->
                        { element = element_, index = index } |> elementAlter |> .element
                    )
        }


{-| at: Structure Preserving accessor over List members.

    import Accessors exposing (view)
    import List.Accessor as List
    import Record

    bars : List { bar : String }
    bars =
        [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars |> view (List.element 1)
    --> Just { bar = "Things" }

    bars |> view (List.element 9000)
    --> Nothing

    bars |> view (List.element 0 << Record.bar)
    --> Just "Stuff"

    bars |> over (List.element 0 << Record.bar) (\_ -> "Whatever")
    --> [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars |> over (List.element 9000 << Record.bar) (\_ -> "Whatever")
    --> bars

-}
element :
    ( DirectionLinear, Int )
    -> Optional (List element) element focusFocus focusFocusView
element focusLocation =
    Accessor.optional
        { name =
            "element " ++ (focusLocation |> Linear.locationToString)
        , view =
            \list ->
                case list |> List.Linear.element focusLocation of
                    Err (ExpectedIndexForLength _) ->
                        Nothing

                    Ok elementFound ->
                        elementFound |> Just
        , map =
            \alter ->
                List.Linear.elementAlter ( focusLocation, alter )
        }
