module Array.Accessor exposing (elementEach, elementIndexEach, element)

{-| Accessors for `Array`s.

@docs elementEach, elementIndexEach, element

-}

import Accessor exposing (Prism, PrismKeepingFocusType, Traversal, lens, onJust, prism, traversal)
import Array exposing (Array)
import Array.Linear
import Linear exposing (DirectionLinear, ExpectedIndexInRange(..))
import Linear.Extra as Linear


{-| This accessor combinator lets you view values inside Array.

    import Array exposing (Array)
    import Accessors exposing (every, view, map)
    import Record

    fooBarray : { foo : Array { bar : Int } }
    fooBarray =
        { foo =
            Array.fromList [ { bar = 2 }, { bar = 3 }, { bar = 4 } ]
        }

    view (Record.foo << every << Record.bar) fooBarray
    --> Array.fromList [ 2, 3, 4 ]

    map (Record.foo << every << Record.bar) ((+) 1) fooBarray
    --> { foo = Array.fromList [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
elementEach :
    Traversal
        (Array element)
        element
        (Array elementMapped)
        elementMapped
        focusFocusNamed
        (Array elementFocusView)
        elementFocus
        elementFocusMapped
        focusFocusNamed
        elementFocusView
        focusFocusFocusNamed
elementEach =
    traversal
        { description = { structure = "Array", focus = "element each" }
        , view = Array.map
        , map = Array.map
        , focusName = identity
        }


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (view, mapOver)
    import Tuple.Accessor as Tuple
    import Record
    import Array.Accessor as Array
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

    fooBarray |> view (Record.foo << Array.elementEach)
    --> Array.fromList
    -->     [ { index = 0, element = { bar = 2 } }
    -->     , { index = 1, element = { bar = 3 } }
    -->     , { index = 2, element = { bar = 4 } }
    -->     ]

    fooBarray
        |> mapOver
            (Record.foo << Array.elementEach)
            (\{ index, element } ->
                case index of
                    0 ->
                        element

                    _ ->
                        { bar = element.bar * 10 }
            )
    --> { foo = Array.fromList [ { bar = 2 }, { bar = 30 }, { bar = 40 } ] }

    fooBarray
        |> view (Record.foo << Array.elementEach << Tuple.second << Record.bar)
    --> Array.fromList [ 2, 3, 4 ]

    fooBarray
        |> mapOver
            (Record.foo << Array.elementEach << Tuple.second << Record.bar)
            ((+) 1)
    --> { foo = Array.fromList [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
elementIndexEach :
    Traversal
        (Array element)
        { element : element, index : Int }
        (Array elementMapped)
        { element : elementMapped, index : Int }
        focusFocusNamed
        (Array elementFocusView)
        elementView
        elementFocus
        focusFocusNamed
        elementFocusView
        focusFocusFocusNamed
elementIndexEach =
    Accessor.traversal
        { description = { structure = "Array", focus = "{element,index} each" }
        , view =
            \elementFocusView ->
                Array.indexedMap
                    (\index element_ ->
                        { element = element_, index = index } |> elementFocusView
                    )
        , map =
            \elementMap ->
                Array.indexedMap
                    (\index element_ ->
                        { element = element_, index = index } |> elementMap |> .element
                    )
        , focusName = identity
        }


{-| Focus an `Array` element at a given index in a [direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import Array exposing (Array)
    import Accessors exposing (view)
    import Array.Accessor as Array
    import Record

    barray : Array { bar : String }
    barray =
        Array.fromList [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray |> view (Array.element 1)
    --> Just { bar = "Things" }

    barray |> view (Array.element ( Up, 9000 ))
    --> Nothing

    barray |> view (Array.element ( Down, 0 ) << Record.bar)
    --> Just "Woot"

    barray
        |> mapOver (Array.element ( Up, 0 ) << Record.bar) (\_ -> "Whatever")
    --> Array.fromList
    -->     [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray
        |> mapOver (Array.element ( Up, 9000 ) << Record.bar) (\_ -> "Whatever")
    --> barray

-}
element :
    ( DirectionLinear, Int )
    ->
        PrismKeepingFocusType
            (Array element)
            element
            { element : focusFocusNamed }
            focusFocus
            focusFocusMapped
            focusFocusNamed
            focusFocusView
            focusFocusFocusNamed
element location =
    prism
        { description =
            { structure = "Array"
            , focus = "element " ++ (location |> Linear.locationToString)
            }
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
        , focusName =
            \focusFocusNamed -> { element = focusFocusNamed }
        }
