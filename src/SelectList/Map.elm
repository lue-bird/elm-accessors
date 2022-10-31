module SelectList.Map exposing (each, selected)

{-| [`Map`](Map#Map) a [`miyamoen/select-list`](https://dark.elm.dmy.fr/packages/miyamoen/select-list/latest/)

@docs each, selected

-}

import Map exposing (Alter, Map)
import SelectList exposing (SelectList)


{-| Map each element contained inside a `SelectList`

    import Map
    import SelectList.Map
    import Record
    import SelectList exposing (SelectList)

    model : { projects : SelectList { id : Int } }
    model =
        { projects =
            SelectList.fromLists
                [ { id = 1 } ]
                { id = 2 }
                [ { id = 3 }, { id = 4 } ]
        }

    model
        |> Map.over
            (Record.projects << SelectList.Map.each << Record.id)
            (\n -> n - 1)
    --> { projects =
    -->     SelectList.fromLists
    -->         [ { id = 0 } ]
    -->         { id = 1 }
    -->         [ { id = 2 }, { id = 3 } ]
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

    model : { projects : SelectList { id : Int } }
    model =
        { projects =
            SelectList.fromLists
                [ { id = 1 } ]
                { id = 2 }
                [ { id = 3 }, { id = 4 } ]
        }

    model
        |> Map.over
            (Record.projects << SelectList.Map.selected << Record.id)
            (\_ -> 37)
    --> { projects =
    -->     SelectList.fromLists
    -->         [ { id = 1 } ]
    -->         { id = 37 }
    -->         [ { id = 3 }, { id = 4 } ]
    --> }

    model
        |> Map.over
            (Record.projects << SelectList.Map.selected << Record.id)
            (\n -> n * 10)
    --> { projects =
    -->     SelectList.fromLists
    -->         [ { id = 1 } ]
    -->         { id = 20 }
    -->         [ { id = 3 }, { id = 4 } ]
    --> }

-}
selected : Alter (SelectList elementMapped) elementMapped
selected =
    Map.at "selected" SelectList.updateSelected
