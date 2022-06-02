module SelectList.Accessor exposing (elementEach, elementIndexEach, selected)

{-| This module exposes some helpers for [`miyamoen/select-list`](https://dark.elm.dmy.fr/packages/miyamoen/select-list/latest/)

@docs elementEach, elementIndexEach, selected

-}

import Accessor exposing (Relation, mapOver, view)
import SelectList exposing (SelectList)


{-| This accessor combinator lets you view values inside List.

    import Accessors exposing (..)
    import Accessors.SelectList as SL
    import Record
    import SelectList exposing (SelectList)

    listRecord : { foo : SelectList { bar : Int } }
    listRecord =
        { foo = SelectList.fromLists [ { bar = 1 } ] { bar = 2 } [ { bar = 3 }, { bar = 4 } ]
        }

    view (Record.foo << SL.each << Record.bar) listRecord
    --> SelectList.fromLists [1] 2 [3, 4]

    map (Record.foo << SL.each << Record.bar) ((+) 1) listRecord
    --> { foo = SelectList.fromLists [ { bar = 2 } ] { bar = 3 } [ { bar = 4 }, { bar = 5 } ] }

-}
elementEach : Relation attribute built transformed -> Relation (SelectList attribute) built (SelectList transformed)
elementEach =
    Accessor.create1ToN
        { description = { structure = "SelectList", focus = "element each" }
        , view = SelectList.map
        , map = SelectList.map
        }


{-| Traverse a `SelectList` including the absolute index of each element

    import Accessors exposing (view, map)
    import Tuple.Accessor as Tuple
    import Accessors.SelectList as SelectList
    import Record
    import SelectList exposing (SelectList)

    listRecord : { foo : SelectList { bar : Int } }
    listRecord =
        { foo =
            SelectList.fromLists
                [ { bar = 1 } ]
                { bar = 2 }
                [ { bar = 3 }, { bar = 4 } ]
        }

    multiplyIfGTOne : ( Int, { bar : Int } ) -> ( Int, { bar : Int } )
    multiplyIfGTOne ( idx, ({ bar } as record) ) =
        if idx > 0 then
            ( idx, { bar = bar * 10 } )
        else
            ( idx, record )


    view (Record.foo << SelectList.elementIndexEach) listRecord
    --> SelectList.fromLists
    -->     [ ( 0, { bar = 1 } ) ]
    -->     ( 1, { bar = 2 } )
    -->     [ ( 2, { bar = 3 } ), ( 3, { bar = 4 } ) ]

    map (Record.foo << SelectList.elementIndexEach) multiplyIfGTOne listRecord
    --> { foo = SelectList.fromLists [ { bar = 1 } ] { bar = 20 } [ { bar = 30 }, { bar = 40 } ] }

    view (Record.foo << SelectList.elementIndexEach << Record.element << Record.bar) listRecord
    --> SelectList.fromLists [1] 2 [3, 4]

    map (Record.foo << SelectList.elementIndexEach << Record.element << Record.bar) ((+) 1) listRecord
    --> { foo = SelectList.fromLists [ { bar = 2 } ] { bar = 3 } [ { bar = 4 }, { bar = 5 } ] }

-}
elementIndexEach : Relation { index : Int, element : element } reachable built -> Relation (SelectList element) reachable (SelectList built)
elementIndexEach =
    Accessor.create1ToN
        { description = { structure = "SelectList", focus = "{ element, index } each" }
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

    import Accessors exposing (..)
    import Accessors.SelectList as SL
    import Record
    import SelectList exposing (SelectList)

    listRecord : { foo : SelectList { bar : Int } }
    listRecord =
        { foo =
            SelectList.fromLists
                [ { bar = 1 } ] { bar = 2 } [ { bar = 3 }, { bar = 4 } ]
        }

    multiplyIfGTOne : ( Int, { bar : Int } ) -> ( Int, { bar : Int } )
    multiplyIfGTOne ( idx, ({ bar } as record) ) =
        if idx > 0 then
            ( idx, { bar = bar * 10 } )
        else
            ( idx, record )

    listRecord |> view (Record.foo << SL.selected << Record.bar)
    --> 2

    listRecord |> mapOver (Record.foo << SL.selected << Record.bar) (\_ -> 37)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 37 } [ { bar = 3 }, { bar = 4 } ]
    --> }

    listRecord |> mapOver (Record.foo << SL.selected << Record.bar) ((*) 10)
    --> { foo =
    -->     SelectList.fromLists
    -->         [ { bar = 1 } ] { bar = 20 } [ { bar = 3 }, { bar = 4 } ]
    --> }

-}
selected : Relation attribute reachable built -> Relation (SelectList attribute) reachable built
selected =
    Accessor.create1To1
        { description = { structure = "SelectList", focus = "selected" }
        , view = SelectList.selected
        , map = SelectList.updateSelected
        }
