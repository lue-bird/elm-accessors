module List.Accessor exposing (elementAt, elementEach, elementIndexEach)

{-| Accessors for `List`s.

@docs elementAt, elementEach, elementIndexEach

-}

import Accessor exposing (Relation, create1To1, create1ToN, onJust)


{-| This accessor combinator lets you view values inside List.

    import Accessors exposing (each, view, map)
    import Record

    listRecord : { foo : List { bar : Int } }
    listRecord =
        { foo =
            [ { bar = 2 }
            , { bar = 3 }
            , { bar = 4 }
            ]
        }

    view (Record.foo << each << Record.bar) listRecord
    --> [2, 3, 4]

    map (Record.foo << each << Record.bar) ((+) 1) listRecord
    --> { foo = [ { bar = 3 }, { bar = 4}, { bar = 5 } ] }

-}
elementEach : Relation attribute built transformed -> Relation (List attribute) built (List transformed)
elementEach =
    create1ToN
        { description = { structure = "List", focus = "element each" }
        , view = List.map
        , map = List.map
        }


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (view, map)
    import List.Accessor as List
    import Tuple.Accessor as Tuple
    import Record

    listRecord : { foo : List { bar : Int } }
    listRecord =
        { foo =
            [ { bar = 2 }
            , { bar = 3 }
            , { bar = 4 }
            ]
        }

    multiplyIfGTOne : ( Int, { bar : Int } ) -> ( Int, { bar : Int } )
    multiplyIfGTOne ( idx, ({ bar } as record) ) =
        if idx > 0 then
            ( idx, { bar = bar * 10 } )

        else
            ( idx, record )


    view (Record.foo << List.elementIndexEach) listRecord
    --> [ ( 0, { bar = 2 } ), ( 1, { bar = 3 } ), ( 2, { bar = 4 } ) ]

    map (Record.foo << List.elementIndexEach) multiplyIfGTOne listRecord
    --> { foo = [ { bar = 2 }, { bar = 30 }, { bar = 40 } ] }

    view (Record.foo << List.elementIndexEach << Record.element << Record.bar) listRecord
    --> [2, 3, 4]

    map (Record.foo << List.elementIndexEach << Record.element << Record.bar) ((+) 1) listRecord
    --> { foo = [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
elementIndexEach : Relation { index : Int, element : element } reachable built -> Relation (List element) reachable (List built)
elementIndexEach =
    create1ToN
        { description = { structure = "List", focus = "{ element, index } each" }
        , view =
            \elementAlter ->
                List.indexedMap
                    (\index element ->
                        { element = element, index = index } |> elementAlter
                    )
        , map =
            \elementAlter ->
                List.indexedMap
                    (\index element ->
                        { element = element, index = index } |> elementAlter |> .element
                    )
        }


{-| at: Structure Preserving accessor over List members.

    import Accessors exposing (view)
    import List.Accessor as List
    import Record

    bars : List { bar : String }
    bars =
        [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars |> view (List.elementAt 1)
    --> Just { bar = "Things" }

    bars |> view (List.elementAt 9000)
    --> Nothing

    bars |> view (List.elementAt 0 << Record.bar)
    --> Just "Stuff"

    bars |> mapOver (List.elementAt 0 << Record.bar) (\_ -> "Whatever")
    --> [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars |> mapOver (List.elementAt 9000 << Record.bar) (\_ -> "Whatever")
    --> bars

-}
elementAt : Int -> Relation v reachable wrap -> Relation (List v) reachable (Maybe wrap)
elementAt focusIndex =
    Accessor.create1To1
        { description = { structure = "List", focus = "element at " ++ (focusIndex |> String.fromInt) }
        , view =
            if focusIndex < 0 then
                \_ -> Nothing

            else
                \list -> list |> List.drop focusIndex |> List.head
        , map =
            \alter list ->
                -- NOTE: `<< onJust` at the end ensures we can't delete any existing keys
                -- so `List.filterMap identity` should be safe
                list
                    |> List.indexedMap
                        (\index ->
                            if index == focusIndex then
                                \v -> Just v |> alter

                            else
                                Just
                        )
                    |> List.filterMap identity
        }
        << onJust
