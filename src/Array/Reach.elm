module Array.Reach exposing (elementEach, elementIndexEach, element)

{-| Reach for `Array`s.

@docs elementEach, elementIndexEach, element

-}

import Array exposing (Array)
import Array.Linear
import Linear exposing (DirectionLinear, ExpectedIndexInRange(..))
import Reach


{-| Reach all elements contained inside an `Array`

    import Array exposing (Array)
    import Reach exposing (every, view, map)
    import Record

    fooBarray : { foo : Array { bar : Int } }
    fooBarray =
        { foo =
            Array.fromList [ { bar = 2 }, { bar = 3 }, { bar = 4 } ]
        }

    fooBarray
        |> Reach.view (Record.foo << every << Record.bar)
    --> Array.fromList [ 2, 3, 4 ]

    fooBarray
        |> Reach.mapOver (Record.foo << every << Record.bar) ((+) 1)
    --> { foo = Array.fromList [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
elementEach :
    Reach.Elements
        (Array element)
        element
        (Array elementView)
        elementView
        (Array elementMapped)
        elementMapped
elementEach =
    Reach.elements "element each"
        { view = Array.map
        , map = Array.map
        }


{-| Reach each element contained inside a `List` including the index of each element

    import Reach exposing (view, mapOver)
    import Tuple.Reach as Tuple
    import Record
    import Array.Reach as Array
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

    fooBarray |> Reach.view (Record.foo << Array.elementEach)
    --> Array.fromList
    -->     [ { index = 0, element = { bar = 2 } }
    -->     , { index = 1, element = { bar = 3 } }
    -->     , { index = 2, element = { bar = 4 } }
    -->     ]

    fooBarray
        |> Reach.mapOver
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
        |> Reach.view (Record.foo << Array.elementEach << Tuple.second << Record.bar)
    --> Array.fromList [ 2, 3, 4 ]

    fooBarray
        |> Reach.mapOver
            (Record.foo << Array.elementEach << Tuple.second << Record.bar)
            ((+) 1)
    --> { foo = Array.fromList [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
elementIndexEach :
    Reach.Elements
        (Array element)
        { element : element, index : Int }
        (Array reachView)
        reachView
        (Array elementMapped)
        { element : elementMapped, index : Int }
elementIndexEach =
    Reach.elements "{element,index} each"
        { view =
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
        }


{-| Focus an `Array` element at a given index in a [direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import Array exposing (Array)
    import Reach exposing (view)
    import Array.Reach as Array
    import Record

    barray : Array { bar : String }
    barray =
        Array.fromList [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray |> Reach.view (Array.element 1)
    --> Just { bar = "Things" }

    barray |> Reach.view (Array.element ( Up, 9000 ))
    --> Nothing

    barray |> Reach.view (Array.element ( Down, 0 ) << Record.bar)
    --> Just "Woot"

    barray
        |> Reach.mapOver (Array.element ( Up, 0 ) << Record.bar) (\_ -> "Whatever")
    --> Array.fromList
    -->     [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray
        |> Reach.mapOver
            (Array.element ( Up, 9000 ) << Record.bar)
            (\_ -> "Whatever")
    --> barray

-}
element :
    ( DirectionLinear, Int )
    ->
        Reach.Maybe
            (Array reachMapped)
            reachMapped
            reachView
            (Array reachMapped)
            reachMapped
element location =
    Reach.maybe ("element " ++ (location |> Linear.locationToString))
        { access =
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
