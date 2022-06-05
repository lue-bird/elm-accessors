module SelectList.Accessor exposing (elementEach, elementIndexEach, selected)

{-| This module exposes some helpers for [`miyamoen/select-list`](https://dark.elm.dmy.fr/packages/miyamoen/select-list/latest/)

@docs elementEach, elementIndexEach, selected

-}

import Accessor exposing (Lens, Traversal, over, get)
import SelectList exposing (SelectList)


{-| This accessor combinator lets you view values inside List.

    import Accessors exposing (..)
    import Accessors.SelectList as SL
    import Record
    import SelectList exposing (SelectList)

    fooBarScroll : { foo : SelectList { bar : Int } }
    fooBarScroll =
        { foo = SelectList.fromLists [ { bar = 1 } ] { bar = 2 } [ { bar = 3 }, { bar = 4 } ]
        }

    view (Record.foo << SL.each << Record.bar) fooBarScroll
    --> SelectList.fromLists [1] 2 [3, 4]

    map (Record.foo << SL.each << Record.bar) ((+) 1) fooBarScroll
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
        { get = SelectList.map
        , over = SelectList.map
        , name = "element each"
        }


{-| Traverse a `SelectList` including the absolute index of each element

    import Accessors exposing (view, over)
    import Tuple.Accessor as Tuple
    import Accessors.SelectList as SelectList
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

    view (Record.foo << SelectList.elementIndexEach) fooBarScroll
    --> SelectList.fromLists
    -->     [ ( 0, { bar = 1 } ) ]
    -->     ( 1, { bar = 2 } )
    -->     [ ( 2, { bar = 3 } ), ( 3, { bar = 4 } ) ]

    fooBarScroll
        |> over
            (Record.foo << SelectList.elementIndexEach)
            (\{ index, element } =
                case index of
                    0 ->
                        element

                    _ ->
                        { bar = element.bar * 10 }
            )
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 20 } [ { bar = 30 }, { bar = 40 } ]
    --> }

    fooBarScroll
        |> view (Record.foo << SelectList.elementIndexEach << Record.element << Record.bar)
    --> SelectList.fromLists [ 1 ] 2 [ 3, 4 ]

    fooBarScroll
        |> over
            (Record.foo << SelectList.elementIndexEach << Record.element << Record.bar)
            ((+) 1)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 2 } ] { bar = 3 } [ { bar = 4 }, { bar = 5 } ] #
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
        { name = "{element,index} each"
        , get =
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
        , over =
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

    import Accessors exposing (..)
    import Accessors.SelectList as SL
    import Record
    import SelectList exposing (SelectList)

    fooBarScroll : { foo : SelectList { bar : Int } }
    fooBarScroll =
        { foo =
            SelectList.fromLists
                [ { bar = 1 } ] { bar = 2 } [ { bar = 3 }, { bar = 4 } ]
        }

    fooBarScroll |> view (Record.foo << SL.selected << Record.bar)
    --> 2

    fooBarScroll |> over (Record.foo << SL.selected << Record.bar) (\_ -> 37)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 37 } [ { bar = 3 }, { bar = 4 } ]
    --> }

    fooBarScroll |> over (Record.foo << SL.selected << Record.bar) ((*) 10)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 20 } [ { bar = 3 }, { bar = 4 } ]
    --> }

-}
selected : Lens (SelectList element) element elementFocus elementFocusView
selected =
    Accessor.lens
        { view = SelectList.selected
        , over = SelectList.updateSelected
        , name = "selected"
        }
