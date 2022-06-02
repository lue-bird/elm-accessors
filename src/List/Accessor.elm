module List.Accessor exposing (elementAt, elementEach, elementIndexEach)

{-| Accessors for `List`s.

@docs elementAt, elementEach, elementIndexEach

-}

import Accessor exposing (Relation, for1To1, for1ToN, onJust)


{-| This accessor combinator lets you access values inside List.

    import Accessors exposing (each, access, map)
    import Field

    listRecord : { foo : List { bar : Int } }
    listRecord =
        { foo =
            [ { bar = 2 }
            , { bar = 3 }
            , { bar = 4 }
            ]
        }

    access (Field.foo << each << Field.bar) listRecord
    --> [2, 3, 4]

    map (Field.foo << each << Field.bar) ((+) 1) listRecord
    --> { foo = [ { bar = 3 }, { bar = 4}, { bar = 5 } ] }

-}
elementEach : Relation attribute built transformed -> Relation (List attribute) built (List transformed)
elementEach =
    for1ToN
        { description = { structure = "List", focus = "element each" }
        , access = List.map
        , map = List.map
        }


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (access, map)
    import List.Accessor as List
    import Tuple.Accessor as Tuple
    import Field

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


    access (Field.foo << List.elementIndexEach) listRecord
    --> [ ( 0, { bar = 2 } ), ( 1, { bar = 3 } ), ( 2, { bar = 4 } ) ]

    map (Field.foo << List.elementIndexEach) multiplyIfGTOne listRecord
    --> { foo = [ { bar = 2 }, { bar = 30 }, { bar = 40 } ] }

    access (Field.foo << List.elementIndexEach << Field.element << Field.bar) listRecord
    --> [2, 3, 4]

    map (Field.foo << List.elementIndexEach << Field.element << Field.bar) ((+) 1) listRecord
    --> { foo = [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
elementIndexEach : Relation { index : Int, element : element } reachable built -> Relation (List element) reachable (List built)
elementIndexEach =
    for1ToN
        { description = { structure = "List", focus = "{ element, index } each" }
        , access =
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

    import Accessors exposing (access)
    import List.Accessor as List
    import Field

    bars : List { bar : String }
    bars =
        [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars |> access (List.elementAt 1)
    --> Just { bar = "Things" }

    bars |> access (List.elementAt 9000)
    --> Nothing

    bars |> access (List.elementAt 0 << Field.bar)
    --> Just "Stuff"

    bars |> map (List.elementAt 0 << Field.bar) (\_ -> "Whatever")
    --> [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    bars |> map (List.elementAt 9000 << Field.bar) (\_ -> "Whatever")
    --> bars

-}
elementAt : Int -> Relation v reachable wrap -> Relation (List v) reachable (Maybe wrap)
elementAt focusIndex =
    Accessor.for1To1
        { description = { structure = "List", focus = "element at " ++ (focusIndex |> String.fromInt) }
        , access =
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
