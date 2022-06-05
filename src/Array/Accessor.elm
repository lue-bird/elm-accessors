module Array.Accessor exposing (elementEach, elementIndexEach, element)

{-| Accessors for `Array`s.

@docs elementEach, elementIndexEach, element

-}

import Accessor exposing (Prism, Traversal, prism, traversal)
import Array exposing (Array)
import Array.Linear
import Linear exposing (DirectionLinear, ExpectedIndexInRange(..))
import Linear.Extra as Linear


{-| This accessor combinator lets you view values inside Array.

    import Array exposing (Array)
    import Accessor exposing (view, mapOver)
    import Record
    import Array.Accessor

    fooBarray : { foo : Array { bar : Int } }
    fooBarray =
        { foo =
            Array.fromList [ { bar = 2 }, { bar = 3 }, { bar = 4 } ]
        }

    fooBarray
        |> view (Record.foo << Array.Accessor.elementEach << Record.bar)
    --> Array.fromList [ 2, 3, 4 ]

    fooBarray
        |> mapOver (Record.foo << Array.Accessor.elementEach << Record.bar) ((+) 1)
    --> { foo = Array.fromList [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
elementEach :
    Traversal
        (Array element)
        element
        (Array elementFocusView)
        elementFocus
        elementFocusView
elementEach =
    traversal
        { description = "element each"
        , view = Array.map
        , map = Array.map
        }


{-| This accessor lets you traverse a list including the index of each element

    import Accessor exposing (view, mapOver)
    import Tuple.Accessor
    import Record
    import Array.Accessor
    import Array exposing (Array)

    fooBarray : { foo : Array { bar : Int } }
    fooBarray =
        { foo =
            Array.fromList
                [ { bar = 2 }
                , { bar = 3 }
                , { bar = 4 }
                ]
        }

    fooBarray |> view (Record.foo << Array.Accessor.elementIndexEach)
    --> Array.fromList
    -->     [ { index = 0, element = { bar = 2 } }
    -->     , { index = 1, element = { bar = 3 } }
    -->     , { index = 2, element = { bar = 4 } }
    -->     ]

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

    fooBarray
        |> mapOver
            (Record.foo << Array.Accessor.elementIndexEach)
            tailMultiplyBy10
    --> { foo =
    -->     Array.fromList
    -->         [ { bar = 2 }, { bar = 30 }, { bar = 40 } ]
    --> }

    fooBarray
        |> view (Record.foo << Array.Accessor.elementIndexEach << Record.element << Record.bar)
    --> Array.fromList [ 2, 3, 4 ]

    fooBarray
        |> mapOver
            (Record.foo << Array.Accessor.elementIndexEach << Record.element << Record.bar)
            ((+) 1)
    --> { foo = Array.fromList [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

In the last 2 examples, `Array.Accessor.elementIndexEach << Record.element` can be replaced with [`Array.Accessor.elementEach`](#elementEach)

-}
elementIndexEach :
    Traversal
        (Array element)
        { element : element, index : Int }
        (Array elementFocusView)
        elementFocus
        elementFocusView
elementIndexEach =
    Accessor.traversal
        { description = "{element,index} each"
        , view =
            \elementView ->
                Array.indexedMap
                    (\index element_ ->
                        { element = element_, index = index } |> elementView
                    )
        , map =
            \elementMap ->
                Array.indexedMap
                    (\index element_ ->
                        { element = element_, index = index } |> elementMap |> .element
                    )
        }


{-| Focus `Array` elements plus their indices.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Linear exposing (DirectionLinear(..))
    import Array exposing (Array)
    import Accessor exposing (view, mapOver)
    import Array.Accessor
    import Record

    barray : Array { bar : String }
    barray =
        Array.fromList [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray |> view (Array.Accessor.element ( Down, 1 ))
    --> Just { bar = "Things" }

    barray |> view (Array.Accessor.element ( Up, 9000 ))
    --> Nothing

    barray |> view (Array.Accessor.element ( Up, 0 ) << Record.bar)
    --> Just "Stuff"

    barray
        |> mapOver
            (Array.Accessor.element ( Up, 0 ) << Record.bar)
            (\_ -> "Whatever")
    --> Array.fromList
    -->     [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray
        |> mapOver
            (Array.Accessor.element ( Up, 9000 ) << Record.bar)
            (\_ -> "Whatever")
    --> barray

-}
element :
    ( DirectionLinear, Int )
    -> Prism (Array element) element focusFocus focusFocusView
element location =
    prism
        { description =
            "element " ++ (location |> Linear.locationToString)
        , view =
            \array ->
                case array |> Array.Linear.element location of
                    Err (ExpectedIndexForLength _) ->
                        Nothing

                    Ok value ->
                        value |> Just
        , map =
            \alter ->
                Array.Linear.elementAlter ( location, alter )
        }
