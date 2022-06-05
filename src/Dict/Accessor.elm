module Dict.Accessor exposing (valueEach, valueKeyEach, valueAt, valueAtString)

{-| Accessors for `Dict`s.

@docs valueEach, valueKeyEach, valueAt, valueAtString

-}

import Accessor exposing (Lens, Traversal, lens, traversal)
import Dict exposing (Dict)


{-| values: This accessor lets you traverse a Dict including the index of each element

    import Accessors exposing (values, map, view)
    import Record
    import Dict exposing (Dict)

    recordDictStringBar : { foo : Dict String { bar : Int } }
    recordDictStringBar =
        { foo =
            Dict.fromList
                [ ( "a", { bar = 2 } )
                , ( "b", { bar = 3 } )
                , ( "c", { bar = 4 } )
                ]
        }

    view (Record.foo << values) recordDictStringBar
    --> Dict.fromList
    -->     [ ( "a", { bar = 2 } ), ( "b", { bar = 3 } ), ( "c", { bar = 4 } ) ]

    map (Record.foo << values << Record.bar) ((*) 10) recordDictStringBar
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 20 } ), ( "b", { bar = 30 } ), ( "c", { bar = 40 } ) ]
    --> }

    view (Record.foo << values << Record.bar) recordDictStringBar
    --> Dict.fromList [ ( "a", 2 ), ( "b", 3 ), ( "c", 4 ) ]

    map (Record.foo << values << Record.bar) ((+) 1) recordDictStringBar
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 3 } ), ( "b", { bar = 4 } ), ( "c", { bar = 5 } ) ]
    --> }

-}
valueEach :
    Traversal
        (Dict comparableKey value)
        value
        (Dict comparableKey valueFocusView)
        valueFocus
        valueFocusView
valueEach =
    traversal
        { description = "value each"
        , view = \valueView -> Dict.map (\_ -> valueView)
        , map = \valueMap -> Dict.map (\_ -> valueMap)
        }


{-| Traverse each `Dict` entry containing its `key` and `value`.

    import Accessors exposing (view, map)
    import Dict.Accessors as Dict
    import Record
    import Dict exposing (Dict)

    recordDictStringBar : { foo : Dict String { bar : Int } }
    recordDictStringBar =
        { foo =
            Dict.fromList
                [ ( "a", { bar = 2 } )
                , ( "b", { bar = 3 } )
                , ( "c", { bar = 4 } )
                ]
        }

    recordDictStringBar |> view (Record.foo << Dict.valueKeyEach)
    --> Dict.fromList
    -->     [ ( "a", ( "a", { bar = 2 } ) )
    -->     , ( "b", ( "b", { bar = 3 } ) )
    -->     , ( "c", ( "c", { bar = 4 } ) )
    -->     ]

    recordDictStringBar
        |> mapOver
            (Record.foo << Dict.valueKeyEach)
            (\entry ->
                { entry
                    | value =
                        case key of
                            "a" ->
                                { bar = entry.value.bar * 10 }

                            _ ->
                                entry
                }
            )
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 20 } ), ( "b", { bar = 3 } ), ( "c", { bar = 4 } ) ]
    --> }

    recordDictStringBar
        |> view (Record.foo << Dict.valueKeyEach << Record.value << Record.bar)
    --> Dict.fromList [ ( "a", 2 ), ( "b", 3 ), ( "c", 4 ) ]

    recordDictStringBar
        |> mapOver
            (Record.foo << Dict.valueKeyEach << Record.value << Record.bar)
            ((+) 1)
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 3 } ), ( "b", { bar = 4 } ), ( "c", { bar = 5 } ) ]
    --> }

-}
valueKeyEach :
    Traversal
        (Dict comparableKey value)
        { key : comparableKey, value : value }
        (Dict comparableKey valueFocusView)
        valueFocus
        valueFocusView
valueKeyEach =
    traversal
        { description = "{key,value} each"
        , view =
            \valueKeyView ->
                Dict.map
                    (\key value ->
                        { key = key, value = value } |> valueKeyView
                    )
        , map =
            \valueKeyMap ->
                Dict.map
                    (\key value ->
                        { key = key, value = value } |> valueKeyMap |> .value
                    )
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

    dict
        |> mapOver
            (Dict.valueAt ( 'x', String.fromChar ) << onJust << Record.bar)
            (\_ -> 3)
    --> dict

[`valueAtString`](#valueAtString) is short for `Dict.Accessor.valueAt ( stringKey, identity )`.

-}
valueAt :
    ( comparableKey, comparableKey -> String )
    -> Lens (Dict comparableKey value) (Maybe value) valueFocus valueFocusView
valueAt ( key, keyToString ) =
    Accessor.lens
        { view = Dict.get key
        , map = Dict.update key
        , description = "value at " ++ (key |> keyToString)
        }


{-| Shorthand for [`Dict.Accessor.valueAt ( "key String", identity )`](#valueAt).
-}
valueAtString :
    String
    -> Lens (Dict String value) (Maybe value) valueFocus valueFocusView
valueAtString key =
    valueAt ( key, identity )
