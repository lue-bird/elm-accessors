module Array.Accessor exposing (elementEach, elementIndexEach, elementAt)

{-| Accessors for `Array`s.

@docs elementEach, elementIndexEach, elementAt

-}

import Accessor exposing (Relation, for1To1, for1ToN, onJust)
import Array exposing (Array)


{-| This accessor combinator lets you view values inside Array.

    import Array exposing (Array)
    import Accessors exposing (every, view, map)
    import Field

    arrayRecord : { foo : Array { bar : Int } }
    arrayRecord =
        { foo =
            Array.fromList [ { bar = 2 }, { bar = 3 }, { bar = 4 } ]
        }

    view (Field.foo << every << Field.bar) arrayRecord
    --> Array.fromList [ 2, 3, 4 ]

    map (Field.foo << every << Field.bar) ((+) 1) arrayRecord
    --> { foo = Array.fromList [ { bar = 3 }, { bar = 4 }, { bar = 5 } ] }

-}
elementEach : Relation attribute built transformed -> Relation (Array attribute) built (Array transformed)
elementEach =
    for1ToN
        { description = { structure = "Array", focus = "element each" }
        , view = Array.map
        , map = Array.map
        }


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (everyIdx, view, map)
    import Tuple.Accessor as Tuple
    import Field
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


    arrayRecord |> view (Field.foo << everyIdx)
    --> Array.fromList
    -->     [ ( 0, { bar = 2 } ), ( 1, { bar = 3 } ), ( 2, { bar = 4 } ) ]

    arrayRecord |> mapOver (Field.foo << everyIdx) multiplyIfGTOne
    --> { foo = Array.fromList [ { bar = 2 }, { bar = 30 }, { bar = 40 } ] }

    arrayRecord |> view (Field.foo << everyIdx << Tuple.second << Field.bar)
    --> Array.fromList [ 2, 3, 4 ]

    arrayRecord
        |> mapOver (Field.foo << everyIdx << Tuple.second << Field.bar) ((+) 1)
    --> { foo = Array.fromList [ { bar = 3 }, { bar = 4 }, { bar = 5 } ]}

-}
elementIndexEach : Relation { element : element, index : Int } reachable built -> Relation (Array element) reachable (Array built)
elementIndexEach =
    for1ToN
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
    import Field

    barray : Array { bar : String }
    barray =
        Array.fromList [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray |> view (elementAt 1)
    --> Just { bar = "Things" }

    barray |> view (elementAt 9000)
    --> Nothing

    barray |> view (elementAt 0 << Field.bar)
    --> Just "Stuff"

    barray |> mapOver (elementAt 0 << Field.bar) (\_ -> "Whatever")
    --> Array.fromList [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray |> mapOver (elementAt 9000 << Field.bar) (\_ -> "Whatever")
    --> barray

-}
elementAt : Int -> Relation v reachable wrap -> Relation (Array v) reachable (Maybe wrap)
elementAt index =
    for1To1
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
