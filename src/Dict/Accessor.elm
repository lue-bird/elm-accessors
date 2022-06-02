module Dict.Accessor exposing (valueEach, valueKeyEach, valueAt, valueAtString)

{-| Accessors for `Dict`s.

@docs valueEach, valueKeyEach, valueAt, valueAtString

-}

import Accessor exposing (Relation, create1To1, create1ToN)
import Dict exposing (Dict)


{-| values: This accessor lets you traverse a Dict including the index of each element

    import Accessors exposing (values, map, view)
    import Record
    import Dict exposing (Dict)

    dictRecord : { foo : Dict String { bar : Int } }
    dictRecord =
        { foo =
            Dict.fromList
                [ ( "a", { bar = 2 } )
                , ( "b", { bar = 3 } )
                , ( "c", { bar = 4 } )
                ]
        }

    view (Record.foo << values) dictRecord
    --> Dict.fromList
    -->     [ ( "a", { bar = 2 } ), ( "b", { bar = 3 } ), ( "c", { bar = 4 } ) ]

    map (Record.foo << values << Record.bar) ((*) 10) dictRecord
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 20 } ), ( "b", { bar = 30 } ), ( "c", { bar = 40 } ) ]
    --> }

    view (Record.foo << values << Record.bar) dictRecord
    --> Dict.fromList [ ( "a", 2 ), ( "b", 3 ), ( "c", 4 ) ]

    map (Record.foo << values << Record.bar) ((+) 1) dictRecord
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 3 } ), ( "b", { bar = 4 } ), ( "c", { bar = 5 } ) ]
    --> }

-}
valueEach : Relation attribute reachable built -> Relation (Dict comparable attribute) reachable (Dict comparable built)
valueEach =
    create1ToN
        { description = { structure = "Dict", focus = "value each" }
        , view = \fn -> Dict.map (\_ -> fn)
        , map = \map -> Dict.map (\_ -> map)
        }


{-| keyed: This accessor lets you traverse a Dict including the index of each element

    import Accessors exposing (view, map, keyed)
    import Tuple.Accessor as Tuple
    import Record
    import Dict exposing (Dict)

    dictRecord : { foo : Dict String { bar : Int } }
    dictRecord =
        { foo =
            Dict.fromList
                [ ( "a", { bar = 2 } )
                , ( "b", { bar = 3 } )
                , ( "c", { bar = 4 } )
                ]
        }

    multiplyIfA : ( String, { bar : Int } ) -> ( String, { bar : Int } )
    multiplyIfA ( key, ({ bar } as record) ) =
        if key == "a" then
            ( key, { bar = bar * 10 } )
        else
            ( key, record )


    view (Record.foo << keyed) dictRecord
    --> Dict.fromList
    -->     [ ( "a", ( "a", { bar = 2 } ) ), ( "b", ( "b", { bar = 3 } ) ), ( "c", ( "c", { bar = 4 } ) ) ]

    map (Record.foo << keyed) multiplyIfA dictRecord
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 20 } ), ( "b", { bar = 3 } ), ( "c", { bar = 4 } ) ]
    --> }

    view (Record.foo << keyed << Tuple.second << Record.bar) dictRecord
    --> Dict.fromList [ ( "a", 2 ), ( "b", 3 ), ( "c", 4 ) ]

    map (Record.foo << keyed << Tuple.second << Record.bar) ((+) 1) dictRecord
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 3 } ), ( "b", { bar = 4 } ), ( "c", { bar = 5 } ) ]
    --> }

-}
valueKeyEach : Relation { key : comparableKey, value : value } reachable built -> Relation (Dict comparableKey value) reachable (Dict comparableKey built)
valueKeyEach =
    create1ToN
        { description = { structure = "Dict", focus = "{ key, value } each" }
        , view =
            \fn -> Dict.map (\key value -> { key = key, value = value } |> fn)
        , map =
            \fn -> Dict.map (\key value -> { key = key, value = value } |> fn |> .value)
        }


{-| key: NON-structure preserving accessor over Dict's

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (view, try)
    import Dict.Accessors as Dict
    import Record

    dict : Dict String { bar : Int }
    dict =
        Dict.fromList [ ( 'b', { bar = 2 } ) ]

    dict |> view (Dict.valueAt ( 'b', String.fromChar ))
    --> Just { bar = 2 }

    dict |> view (Dict.valueAt ( 'b', String.fromChar ))
    --> Nothing

    dict |> view (Dict.valueAt ( 'b', String.fromChar ) << onJust << Record.bar)
    --> Just 2

    dict |> mapOver (Dict.valueAt ( 'b', String.fromChar )) (\_ -> Nothing)
    --> dict |> Dict.remove 'b'

    dict |> mapOver (Dict.valueAt ( 'x', String.fromChar ) << onJust << Record.bar) (\_ -> 3)
    --> dict

[`valueAtString`](#valueAtString) is short for `Dict.Accessor.valueAt ( stringKey, identity )`.

-}
valueAt :
    ( comparableKey, comparableKey -> String )
    -> Relation (Maybe value) reachable wrap
    -> Relation (Dict comparableKey value) reachable wrap
valueAt ( key, keyToString ) =
    Accessor.create1To1
        { description = { structure = "Dict", focus = "value at " ++ (key |> keyToString) }
        , view = Dict.get key
        , map = Dict.update key
        }


{-| Short for [`Dict.Accessor.valueAt ( stringKey, identity )`](#valueAt).
-}
valueAtString :
    String
    -> Relation (Maybe value) reachable wrap
    -> Relation (Dict String value) reachable wrap
valueAtString key =
    valueAt ( key, identity )
