module SelectList.Map exposing (each, selected)

{-| map into a [`miyamoen/select-list`](https://dark.elm.dmy.fr/packages/miyamoen/select-list/latest/)

@docs each, selected

-}

import Map exposing (Alter, Map)
import SelectList exposing (SelectList)


{-| Map each element contained inside a `SelectList`

    import Map
    import SelectList.Map
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
        |> Map.over
            (Record.foo << SelectList.Map.each << Record.bar)
            (\n -> n + 1)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 2 } ]
    -->         { bar = 3 }
    -->         [ { bar = 4 }, { bar = 5 } ]
    --> }

-}
each : Map (SelectList element) element (SelectList elementMapped) elementMapped
each =
    Map.at "each" SelectList.map


{-| Map a `SelectList`'s selected element

    import Map
    import SelectList.Map
    import Record
    import SelectList exposing (SelectList)

    fooBarScroll : { foo : SelectList { bar : Int } }
    fooBarScroll =
        { foo =
            SelectList.fromLists
                [ { bar = 1 } ] { bar = 2 } [ { bar = 3 }, { bar = 4 } ]
        }

    fooBarScroll
        |> Map.over
            (Record.foo << SelectList.Map.selected << Record.bar)
            (\_ -> 37)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 37 } [ { bar = 3 }, { bar = 4 } ]
    --> }

    fooBarScroll
        |> Map.over
            (Record.foo << SelectList.Map.selected << Record.bar)
            (\n -> n * 10)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 20 } [ { bar = 3 }, { bar = 4 } ]
    --> }

-}
selected : Alter (SelectList elementMapped) elementMapped
selected =
    Map.at "selected" SelectList.updateSelected
