module SelectList.Accessor exposing (elementEach, elementIndexEach, selected)

{-| This module exposes some helpers for [`miyamoen/select-list`](https://dark.elm.dmy.fr/packages/miyamoen/select-list/latest/)

@docs elementEach, elementIndexEach, selected

-}

import Accessor exposing (Traversal)
import SelectList exposing (SelectList)


{-| This accessor combinator lets you view values inside List.

    import Accessor exposing (view, mapOver)
    import SelectList.Accessor
    import Record
    import SelectList exposing (SelectList)

    fooBarScroll : { foo : SelectList { bar : Int } }
    fooBarScroll =
        { foo = SelectList.fromLists [ { bar = 1 } ] { bar = 2 } [ { bar = 3 }, { bar = 4 } ]
        }

    fooBarScroll
        |> view (Record.foo << SelectList.Accessor.elementEach << Record.bar)
    --> SelectList.fromLists [1] 2 [3, 4]

    fooBarScroll
        |> mapOver (Record.foo << SelectList.Accessor.elementEach << Record.bar) ((+) 1)
    --> { foo = SelectList.fromLists [ { bar = 2 } ] { bar = 3 } [ { bar = 4 }, { bar = 5 } ] }

-}
elementEach :
    Traversal
        (SelectList element)
        element
        (SelectList elementFocusView)
        elementFocus
        elementFocusView
elementEach =
    Accessor.traversal
        { view = SelectList.map
        , map = SelectList.map
        , description = "element each"
        }


{-| Traverse a `SelectList` including the absolute index of each element

    import Accessor exposing (view, mapOver)
    import SelectList.Accessor
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

    view (Record.foo << SelectList.Accessor.elementIndexEach) fooBarScroll
    --> SelectList.fromLists
    -->     [ { index = 0, element = { bar = 1 } } ]
    -->     { index = 1, element = { bar = 2 } }
    -->     [ { index = 2, element = { bar = 3 } }
    -->     , { index = 3, element = { bar = 4 } }
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

    fooBarScroll
        |> mapOver
            (Record.foo << SelectList.Accessor.elementIndexEach)
            tailMultiplyBy10
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 20 } [ { bar = 30 }, { bar = 40 } ]
    --> }

    fooBarScroll
        |> view (Record.foo << SelectList.Accessor.elementIndexEach << Record.element << Record.bar)
    --> SelectList.fromLists [ 1 ] 2 [ 3, 4 ]

    fooBarScroll
        |> mapOver
            (Record.foo << SelectList.Accessor.elementIndexEach << Record.element << Record.bar)
            ((+) 1)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 2 } ] { bar = 3 } [ { bar = 4 }, { bar = 5 } ]
    --> }

-}
elementIndexEach :
    Traversal
        (SelectList element)
        { index : Int, element : element }
        (SelectList elementFocusView)
        elementFocus
        elementFocusView
elementIndexEach =
    Accessor.traversal
        { description = "{element,index} each"
        , view =
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
                            { element = element, index = index } |> map |> .element
                        )
                        before
                    )
                    ({ index = selectedIndex, element = current } |> map |> .element)
                    (List.indexedMap
                        (\idx element ->
                            { element = element, index = selectedIndex + 1 + idx } |> map |> .element
                        )
                        after
                    )
        }


{-| Accessor on the `SelectList`'s selected element.

    import Accessor exposing (..)
    import SelectList
    import Record
    import SelectList exposing (SelectList)

    fooBarScroll : { foo : SelectList { bar : Int } }
    fooBarScroll =
        { foo =
            SelectList.fromLists
                [ { bar = 1 } ] { bar = 2 } [ { bar = 3 }, { bar = 4 } ]
        }

    fooBarScroll
        |> view (Record.foo << SelectList.Accessor.selected << Record.bar)
    --> 2

    fooBarScroll
        |> mapOver
            (Record.foo << SelectList.Accessor.selected << Record.bar)
            (\_ -> 37)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 37 } [ { bar = 3 }, { bar = 4 } ]
    --> }

    fooBarScroll
        |> mapOver
            (Record.foo << SelectList.Accessor.selected << Record.bar)
            ((*) 10)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 20 } [ { bar = 3 }, { bar = 4 } ]
    --> }

-}
selected :
    Accessor.Relation element focusFocus focusFocusView
    -> Accessor.Relation (SelectList element) focusFocus focusFocusView
selected =
    Accessor.lens
        { view = SelectList.selected
        , map = SelectList.updateSelected
        , description = "selected"
        }
