module SelectList.Reach exposing (elementEach, selected)

{-| Reach into a [`miyamoen/select-list`](https://dark.elm.dmy.fr/packages/miyamoen/select-list/latest/)

@docs elementEach, selected

-}

import Reach
import SelectList exposing (SelectList)


{-| Reach each element contained inside a `SelectList`

    import Reach
    import SelectList.Reach
    import Record
    import SelectList exposing (SelectList)

    fooBarScroll : { foo : SelectList { bar : Int } }
    fooBarScroll =
        { foo =
            SelectList.fromLists
                [ { bar = 1 } ]
                { bar = 2 }
                [ { bar = 3 }, { bar = 4 } ]
        }

    fooBarScroll
        |> Reach.view
            (Record.foo |> Reach.into SelectList.Reach.elementEach |> Reach.into Record.bar)
    --> SelectList.fromLists [1] 2 [3, 4]

    fooBarScroll
        |> Reach.mapOver
            (Record.foo |> Reach.into SelectList.Reach.elementEach |> Reach.into Record.bar)
            (\n -> n + 1)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 2 } ]
    -->         { bar = 3 }
    -->         [ { bar = 4 }, { bar = 5 } ]
    --> }

-}
elementEach :
    Reach.Elements
        (SelectList element)
        element
        (SelectList elementView)
        elementView
        (SelectList elementMapped)
        elementMapped
elementEach =
    Reach.elements "element each"
        { view = SelectList.map
        , map = SelectList.map
        }


{-| Reach the `SelectList`'s selected element

    import Reach
    import SelectList.Reach
    import Record
    import SelectList exposing (SelectList)

    fooBarScroll : { foo : SelectList { bar : Int } }
    fooBarScroll =
        { foo =
            SelectList.fromLists
                [ { bar = 1 } ] { bar = 2 } [ { bar = 3 }, { bar = 4 } ]
        }

    fooBarScroll
        |> Reach.view
            (Record.foo |> Reach.into SelectList.Reach.selected |> Reach.into Record.bar)
    --> 2

    fooBarScroll
        |> Reach.mapOver
            (Record.foo |> Reach.into SelectList.Reach.selected |> Reach.into Record.bar)
            (\_ -> 37)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 37 } [ { bar = 3 }, { bar = 4 } ]
    --> }

    fooBarScroll
        |> Reach.mapOver
            (Record.foo |> Reach.into SelectList.Reach.selected |> Reach.into Record.bar)
            (\n -> n * 10)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 20 } [ { bar = 3 }, { bar = 4 } ]
    --> }

-}
selected :
    Reach.Part
        (SelectList elementMapped)
        elementMapped
        elementView
        (SelectList elementMapped)
        elementMapped
selected =
    Reach.part "selected"
        { access = SelectList.selected
        , map = SelectList.updateSelected
        }
