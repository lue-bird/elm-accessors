module List.Accessor exposing (element, elementEach, elementIndexEach)

{-| Accessors for `List`s.

@docs element, elementEach, elementIndexEach

-}

import Accessor exposing (Prism, Traversal, traversal)
import Linear exposing (DirectionLinear, ExpectedIndexInRange(..))
import Linear.Extra as Linear
import List.Linear


{-| This accessor combinator lets you view values inside List.

    import Accessor exposing (view, mapOver)
    import Record
    import List.Accessor

    listRecord : { foo : List { bar : Int } }
    listRecord =
        { foo =
            [ { bar = 2 }
            , { bar = 3 }
            , { bar = 4 }
            ]
        }

    listRecord
        |> view (Record.foo << List.Accessor.elementEach << Record.bar)
    --> [ 2, 3, 4 ]

    listRecord
        |> mapOver (Record.foo << List.Accessor.elementEach << Record.bar)
        ((+) 1)
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
        { description = "element each"
        , view = List.map
        , map = List.map
        }


{-| This accessor lets you traverse a list including the index of each element

    import Accessor exposing (view, mapOver)
    import List.Accessor
    import Record

    listRecord : { foo : List { bar : Int } }
    listRecord =
        { foo =
            [ { bar = 2 }
            , { bar = 3 }
            , { bar = 4 }
            ]
        }

    listRecord |> view (Record.foo << List.Accessor.elementIndexEach)
    --> [ { index = 0, element = { bar = 2 } }
    --> , { index = 1, element = { bar = 3 } }
    --> , { index = 2, element = { bar = 4 } }
    --> ]

    tailMultiplyBy10 : { index : Int, element : { bar : Int } } -> { index : Int, element : { bar : Int } }
    tailMultiplyBy10 =
        \item ->
            { item
                | element =
                    case item.index of
                        0 ->
                            item.element
                        _ ->
                            { bar = item.element.bar * 10 }
            }

    listRecord
        |> mapOver
            (Record.foo << List.Accessor.elementIndexEach)
            tailMultiplyBy10
    --> { foo = [ { bar = 2 }, { bar = 30 }, { bar = 40 } ] }

    listRecord
        |> view (Record.foo << List.Accessor.elementIndexEach << Record.element << Record.bar)
    --> [ 2, 3, 4 ]

    listRecord
        |> mapOver
            (Record.foo << List.Accessor.elementIndexEach << Record.element << Record.bar)
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
        { description = "{element,index} each"
        , view =
            \elementAlter ->
                List.indexedMap
                    (\index element_ ->
                        { element = element_, index = index } |> elementAlter
                    )
        , map =
            \elementAlter ->
                List.indexedMap
                    (\index element_ ->
                        { element = element_, index = index } |> elementAlter |> .element
                    )
        }


{-| at: Structure Preserving accessor over List members.

    import Linear exposing (DirectionLinear(..))
    import Accessor exposing (view, mapOver)
    import List.Accessor
    import Record

    bars : List { bar : String }
    bars =
        [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars |> view (List.Accessor.element ( Down, 1 ))
    --> Just { bar = "Things" }

    bars |> view (List.Accessor.element ( Up, 9000 ))
    --> Nothing

    bars |> view (List.Accessor.element ( Up, 0 ) << Record.bar)
    --> Just "Stuff"

    bars
        |> mapOver
            (List.Accessor.element ( Up, 0 ) << Record.bar)
            (\_ -> "Whatever")
    --> [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars
        |> mapOver
            (List.Accessor.element ( Up, 9000 ) << Record.bar)
            (\_ -> "Whatever")
    --> bars

-}
element :
    ( DirectionLinear, Int )
    -> Prism (List element) element focusFocus focusFocusView
element focusLocation =
    Accessor.prism
        { description =
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
