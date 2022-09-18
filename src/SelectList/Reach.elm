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
            (Record.foo << SelectList.Reach.elementEach << Record.bar)
    --> SelectList.fromLists [1] 2 [3, 4]

    fooBarScroll
        |> Reach.mapOver
            (Record.foo << SelectList.Reach.elementEach << Record.bar)
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


{-| Reach each element contained inside a `SelectList` including the absolute index of each element.

Both examples ↓ show that this is always the final step
before using the created reach to [map](Reach#mapOver) or [`view`](Reach#view) inside a structure

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
            (Record.foo << SelectList.Reach.elementIndexEach)
    --> SelectList.fromLists
    -->     [ { index = 0, element = { bar = 1 } } ]
    -->     { index = 1, element = { bar = 2 } }
    -->     [ { index = 2, element = { bar = 3 } }
    -->     , { index = 3, element = { bar = 4 } }
    -->     ]

    fooBarScroll
        |> Reach.mapOver
            (Record.foo << SelectList.Reach.elementIndexEach)
            (\node ->
                case node.index of
                    0 ->
                        node.element
                    _ ->
                        { bar = node.element.bar * 10 }
            )
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 20 } [ { bar = 30 }, { bar = 40 } ]
    --> }

-}
elementIndexEach :
    Reach.Elements
        (SelectList element)
        { element : element, index : Int }
        (SelectList elementView)
        elementView
        (SelectList elementMapped)
        elementMapped
elementIndexEach =
    Reach.elements "{element,index} each"
        { view =
            \alter selectList ->
                let
                    ( before, current, after ) =
                        selectList |> SelectList.toTuple

                    currentIndex =
                        selectList |> SelectList.index
                in
                SelectList.fromLists
                    (before
                        |> List.indexedMap
                            (\index element ->
                                { element = element, index = index } |> alter
                            )
                    )
                    ({ element = current, index = currentIndex } |> alter)
                    (List.indexedMap
                        (\index element ->
                            { element = element, index = currentIndex + 1 + index } |> alter
                        )
                        after
                    )
        , map =
            \map selectList ->
                let
                    ( before, current, after ) =
                        selectList |> SelectList.toTuple

                    selectedIndex =
                        selectList |> SelectList.index
                in
                SelectList.fromLists
                    (List.indexedMap
                        (\index element ->
                            { element = element, index = index } |> map
                        )
                        before
                    )
                    ({ index = selectedIndex, element = current } |> map)
                    (List.indexedMap
                        (\idx element ->
                            { element = element, index = selectedIndex + 1 + idx } |> map
                        )
                        after
                    )
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
            (Record.foo << SelectList.Reach.selected << Record.bar)
    --> 2

    fooBarScroll
        |> Reach.mapOver
            (Record.foo << SelectList.Reach.selected << Record.bar)
            (\_ -> 37)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 37 } [ { bar = 3 }, { bar = 4 } ]
    --> }

    fooBarScroll
        |> Reach.mapOver
            (Record.foo << SelectList.Reach.selected << Record.bar)
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
        , description = "selected"
        }
