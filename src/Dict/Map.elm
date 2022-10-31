module Dict.Map exposing (valueEach, valueAt, valueAtString)

{-| map into a `Dict`

@docs valueEach, valueAt, valueAtString

-}

import Dict exposing (Dict)
import Map exposing (Map)


{-| Traverse a Dict including the index of each element

    import Map
    import Dict.Map
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
        |> Map.view (Record.foo << Dict.Map.valueEach)
    --> Dict.fromList
    -->     [ ( "a", { bar = 2 } ), ( "b", { bar = 3 } ), ( "c", { bar = 4 } ) ]

    recordDictStringBar
        |> Map.over (Record.foo << Dict.Map.valueEach << Record.bar) ((*) 10)
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 20 } ), ( "b", { bar = 30 } ), ( "c", { bar = 40 } ) ]
    --> }

    recordDictStringBar
        |> Map.view (Record.foo << Dict.Map.valueEach << Record.bar)
    --> Dict.fromList [ ( "a", 2 ), ( "b", 3 ), ( "c", 4 ) ]

    recordDictStringBar
        |> Map.over (Record.foo << Dict.Map.valueEach << Record.bar) (\n -> n + 1)
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 3 } ), ( "b", { bar = 4 } ), ( "c", { bar = 5 } ) ]
    --> }

-}
valueEach :
    Map
        (Dict key value)
        value
        (Dict key valueMapped)
        valueMapped
valueEach =
    Map.at "value each"
        (\valueMap -> Dict.map (\_ -> valueMap))


{-| map into a `Dict`'s value at a given key

    import Dict exposing (Dict)
    import Map exposing (onJust)
    import Dict.Map
    import Record

    dict : Dict Char { bar : Int }
    dict =
        Dict.fromList [ ( 'b', { bar = 2 } ) ]

    dict |> Map.view (Dict.Map.valueAt ( 'b', String.fromChar ))
    --> Just { bar = 2 }

    dict |> Map.view (Dict.Map.valueAt ( 'a', String.fromChar ))
    --> Nothing

    dict
        |> Map.view
            (Dict.Map.valueAt ( 'b', String.fromChar ) << Record.bar)
    --> Just 2

    dict
        |> Map.over
            (Dict.Map.valueAt ( 'x', String.fromChar ) << Record.bar)
            (\_ -> 3)
    --> dict

[`valueAtString`](#valueAtString) is short for `Dict.Map.valueAt ( stringKey, identity )`.

-}
valueAt :
    ( comparableKey, comparableKey -> String )
    ->
        Map
            (Dict comparableKey value)
            value
            (Dict comparableKey value)
            value
valueAt ( key, keyToString ) =
    Map.at (key |> keyToString)
        (\valueMap structure ->
            structure |> Dict.update key (Maybe.map valueMap)
        )


{-| Shorthand for [`Dict.Map.valueAt ( "key String", identity )`](#valueAt).
-}
valueAtString :
    String
    ->
        Map
            (Dict String value)
            value
            (Dict String value)
            value
valueAtString key =
    valueAt ( key, identity )
