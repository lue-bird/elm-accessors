module Dict.Reach exposing (valueEach, valueKeyEach, valueAt, valueAtString)

{-| Reach for `Dict`s.

@docs valueEach, valueKeyEach, valueAt, valueAtString

-}

import Dict exposing (Dict)
import Reach


{-| Traverse a Dict including the index of each element

    import Reach
    import Dict.Reach
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

    recordDictStringBar
        |> Reach.view (Record.foo << Dict.Reach.valueEach)
    --> Dict.fromList
    -->     [ ( "a", { bar = 2 } ), ( "b", { bar = 3 } ), ( "c", { bar = 4 } ) ]

    recordDictStringBar
        |> Reach.mapOver (Record.foo << Dict.Reach.valueEach << Record.bar) ((*) 10)
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 20 } ), ( "b", { bar = 30 } ), ( "c", { bar = 40 } ) ]
    --> }

    recordDictStringBar
        |> Reach.view (Record.foo << Dict.Reach.valueEach << Record.bar)
    --> Dict.fromList [ ( "a", 2 ), ( "b", 3 ), ( "c", 4 ) ]

    recordDictStringBar
        |> Reach.mapOver (Record.foo << Dict.Reach.valueEach << Record.bar) ((+) 1)
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 3 } ), ( "b", { bar = 4 } ), ( "c", { bar = 5 } ) ]
    --> }

-}
valueEach :
    Reach.Elements
        (Dict key value)
        value
        (Dict key valueView)
        valueView
        (Dict key valueMapped)
        valueMapped
valueEach =
    Reach.elements "value each"
        { view = \valueView -> Dict.map (\_ -> valueView)
        , map = \valueMap -> Dict.map (\_ -> valueMap)
        }


{-| Traverse each `Dict` entry containing its `key` and `value`.

    import Reach exposing (view, map)
    import Dict.Reach as Dict
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

    recordDictStringBar |> Reach.view (Record.foo << Dict.valueKeyEach)
    --> Dict.fromList
    -->     [ ( "a", ( "a", { bar = 2 } ) )
    -->     , ( "b", ( "b", { bar = 3 } ) )
    -->     , ( "c", ( "c", { bar = 4 } ) )
    -->     ]

    recordDictStringBar
        |> Reach.mapOver
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
        |> Reach.view (Record.foo << Dict.valueKeyEach << Record.value << Record.bar)
    --> Dict.fromList [ ( "a", 2 ), ( "b", 3 ), ( "c", 4 ) ]

    recordDictStringBar
        |> Reach.mapOver
            (Record.foo << Dict.valueKeyEach << Record.value << Record.bar)
            ((+) 1)
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 3 } ), ( "b", { bar = 4 } ), ( "c", { bar = 5 } ) ]
    --> }

-}
valueKeyEach :
    Reach.Elements
        (Dict key value)
        { key : key, value : value }
        (Dict key reachView)
        reachView
        (Dict key valueMapped)
        { key : key, value : valueMapped }
valueKeyEach =
    Reach.elements "{key,value} each"
        { view =
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


{-| NON-structure preserving reach into `Dict`s

In terms of reach, think of Dicts as records where each field is a `Maybe`.

    import Dict exposing (Dict)
    import Reach exposing (view, try)
    import Dict.Reach
    import Record

    dict : Dict String { bar : Int }
    dict =
        Dict.fromList [ ( 'b', { bar = 2 } ) ]

    dict |> Reach.view (Dict.Reach.valueAt ( 'b', String.fromChar ))
    --> Just { bar = 2 }

    dict |> Reach.view (Dict.Reach.valueAt ( 'b', String.fromChar ))
    --> Nothing

    dict
        |> Reach.view
            (Dict.Reach.valueAt ( 'b', String.fromChar ) << onJust << Record.bar)
    --> Just 2

    dict
        |> Reach.mapOver
            (Dict.Reach.valueAt ( 'b', String.fromChar ))
            (\_ -> Nothing)
    --> dict |> Dict.remove 'b'

    dict
        |> Reach.mapOver
            (Dict.Reach.valueAt ( 'x', String.fromChar ) << onJust << Record.bar)
            (\_ -> 3)
    --> dict

[`valueAtString`](#valueAtString) is short for `Dict.Reach.valueAt ( stringKey, identity )`.

-}
valueAt :
    ( comparableKey, comparableKey -> String )
    ->
        Reach.Part
            (Dict comparableKey value)
            (Maybe value)
            valueView
            (Dict comparableKey value)
            (Maybe value)
valueAt ( key, keyToString ) =
    Reach.part ("value at " ++ (key |> keyToString))
        { access = Dict.get key
        , map = Dict.update key
        }


{-| Shorthand for [`Dict.Reach.valueAt ( "key String", identity )`](#valueAt).
-}
valueAtString :
    String
    ->
        Reach.Part
            (Dict String value)
            (Maybe value)
            valueView
            (Dict String value)
            (Maybe value)
valueAtString key =
    valueAt ( key, identity )
