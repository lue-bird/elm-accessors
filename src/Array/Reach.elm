module Array.Reach exposing (elementEach, element)

{-| Reach into an `Array`

@docs elementEach, element

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
        |> Reach.view (Record.foo |> Reach.into Array.Reach.elementEach |> Reach.into Record.bar)
    --> Array.fromList [ 2, 3, 4 ]

    fooBarray
        |> Reach.mapOver
            (Record.foo |> Reach.into Array.Reach.elementEach |> Reach.into Record.bar)
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

    barray |> Reach.view (Array.Reach.element 2 |> Reach.into Record.bar)
    --> Just "Woot"

    barray
        |> Reach.mapOver (Array.Reach.element 0 |> Reach.into Record.bar) (\_ -> "Whatever")
    --> Array.fromList
    -->     [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray
        |> Reach.mapOver
            (Array.Reach.element 9000 |> Reach.into Record.bar)
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
