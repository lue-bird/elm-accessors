module Array.Reach exposing (elementEach, elementIndexEach, element)

{-| Reach into an `Array`

@docs elementEach, elementIndexEach, element

-}

import Array exposing (Array)
import Reach


{-| Reach each element contained inside an `Array`

    import Array exposing (Array)
    import Reach
    import Array.Reach
    import Record

    fooBarray : { foo : Array { bar : Int } }
    fooBarray =
        { foo =
            Array.fromList [ { bar = 2 }, { bar = 3 }, { bar = 4 } ]
        }

    fooBarray
        |> Reach.view (Record.foo << Array.Reach.elementEach << Record.bar)
    --> Array.fromList [ 2, 3, 4 ]

    fooBarray
        |> Reach.mapOver
            (Record.foo << Array.Reach.elementEach << Record.bar)
            (\n -> n + 1)
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


{-| Reach each element contained inside an `Array` including the index of each element.

Both examples ↓ show that this is always the final step
before using the created reach to [map](Reach#mapOver) or [`view`](Reach#view) inside a structure

    import Reach
    import Record
    import Array.Reach
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

    fooBarray |> Reach.view (Record.foo << Array.Reach.elementIndexEach)
    --> Array.fromList
    -->     [ { index = 0, element = { bar = 2 } }
    -->     , { index = 1, element = { bar = 3 } }
    -->     , { index = 2, element = { bar = 4 } }
    -->     ]

    fooBarray
        |> Reach.mapOver
            (Record.foo << Array.Reach.elementIndexEach)
            (\{ index, element } ->
                case index of
                    0 ->
                        element
                    _ ->
                        { bar = element.bar * 10 }
            )
    --> { foo =
    -->     Array.fromList [ { bar = 2 }, { bar = 30 }, { bar = 40 } ]
    --> }

-}
elementIndexEach :
    Reach.Elements
        (Array element)
        { element : element, index : Int }
        (Array reachView)
        reachView
        (Array elementMapped)
        elementMapped
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
                        { element = element_, index = index } |> elementMap
                    )
        }


{-| Focus an `Array` element at a given index in a [direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/).

    import Array exposing (Array)
    import Reach
    import Array.Reach
    import Record

    barray : Array { bar : String }
    barray =
        Array.fromList [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray |> Reach.view (Array.Reach.element 1)
    --> Just { bar = "Things" }

    barray |> Reach.view (Array.Reach.element 9000)
    --> Nothing

    barray |> Reach.view (Array.Reach.element 2 << Record.bar)
    --> Just "Woot"

    barray
        |> Reach.mapOver (Array.Reach.element 0 << Record.bar) (\_ -> "Whatever")
    --> Array.fromList
    -->     [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray
        |> Reach.mapOver
            (Array.Reach.element 9000 << Record.bar)
            (\_ -> "Whatever")
    --> barray

-}
element :
    Int
    ->
        Reach.Maybe
            (Array reachMapped)
            reachMapped
            reachView
            (Array reachMapped)
            reachMapped
element index =
    Reach.maybe
        ("element "
            ++ (index |> String.fromInt)
        )
        { access =
            \array ->
                array |> Array.get index
        , map =
            \alter array ->
                case array |> Array.get index of
                    Nothing ->
                        array

                    Just elementAtIndex ->
                        array |> Array.set index (elementAtIndex |> alter)
        }
