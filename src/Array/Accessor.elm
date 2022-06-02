module Array.Accessor exposing (elementEach, elementIndexEach, elementAt)

{-| Accessors for `Array`s.

@docs elementEach, elementIndexEach, elementAt

-}

import Accessor exposing (Relation, create1To1, create1ToN, onJust)
import Array exposing (Array)


{-| This accessor combinator lets you view values inside Array.

    import Array exposing (Array)
    import Accessors exposing (every, view, map)
    import Record

    arrayRecord : { foo : Array { bar : Int } }
    arrayRecord =
        { foo =
            Array.fromList [ { bar = 2 }, { bar = 3 }, { bar = 4 } ]
        }

    view (Record.foo << every << Record.bar) arrayRecord
    --> Array.fromList [ 2, 3, 4 ]

    map (Record.foo << every << Record.bar) ((+) 1) arrayRecord
    --> { foo = Array.fromList [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
elementEach : Relation attribute built transformed -> Relation (Array attribute) built (Array transformed)
elementEach =
    create1ToN
        { description = { structure = "Array", focus = "element each" }
        , view = Array.map
        , map = Array.map
        }


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (everyIdx, view, map)
    import Tuple.Accessor as Tuple
    import Record
    import Array exposing (Array)

    arrayRecord : { foo : Array { bar : Int } }
    arrayRecord =
        { foo =
            Array.fromList
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


    arrayRecord |> view (Record.foo << everyIdx)
    --> Array.fromList
    -->     [ ( 0, { bar = 2 } ), ( 1, { bar = 3 } ), ( 2, { bar = 4 } ) ]

    arrayRecord |> mapOver (Record.foo << everyIdx) multiplyIfGTOne
    --> { foo = Array.fromList [ { bar = 2 }, { bar = 30 }, { bar = 40 } ] }

    arrayRecord |> view (Record.foo << everyIdx << Tuple.second << Record.bar)
    --> Array.fromList [ 2, 3, 4 ]

    arrayRecord
        |> mapOver (Record.foo << everyIdx << Tuple.second << Record.bar) ((+) 1)
    --> { foo = Array.fromList [ { bar = 3 }, { bar = 4 }, { bar = 5 } ]}

-}
elementIndexEach : Relation { element : element, index : Int } reachable built -> Relation (Array element) reachable (Array built)
elementIndexEach =
    create1ToN
        { description = { structure = "Array", focus = "{ element, index } each" }
        , view =
            \fn ->
                Array.indexedMap
                    (\index element -> { element = element, index = index } |> fn)
        , map =
            \fn ->
                Array.indexedMap
                    (\index element -> { element = element, index = index } |> fn |> .element)
        }


{-| This accessor combinator lets you view Array indices.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Array exposing (Array)
    import Accessors exposing (view)
    import Array.Accessor exposing (elementAt)
    import Record

    barray : Array { bar : String }
    barray =
        Array.fromList [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray |> view (elementAt 1)
    --> Just { bar = "Things" }

    barray |> view (elementAt 9000)
    --> Nothing

    barray |> view (elementAt 0 << Record.bar)
    --> Just "Stuff"

    barray |> mapOver (elementAt 0 << Record.bar) (\_ -> "Whatever")
    --> Array.fromList [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray |> mapOver (elementAt 9000 << Record.bar) (\_ -> "Whatever")
    --> barray

-}
elementAt : Int -> Relation v reachable wrap -> Relation (Array v) reachable (Maybe wrap)
elementAt index =
    create1To1
        { description = { structure = "Array", focus = "element at " ++ (index |> String.fromInt) }
        , view = Array.get index
        , map =
            \alter array ->
                -- NOTE: `<< onJust` at the end ensures we can't delete any existing keys
                -- so `List.filterMap identity` should be safe
                -- TODO: use LinearDirection
                array
                    |> Array.indexedMap
                        (\idx_ v ->
                            if index == idx_ then
                                alter (Just v)

                            else
                                Just v
                        )
                    |> Array.foldl
                        (\element acc ->
                            case element of
                                Just v ->
                                    Array.push v acc

                                Nothing ->
                                    acc
                        )
                        Array.empty
        }
        << onJust
