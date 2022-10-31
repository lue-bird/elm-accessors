module Dict.Map exposing (valueEach, valueAt, valueAtString)

{-| [`Map`](Map#Map) a `Dict`

@docs valueEach, valueAt, valueAtString

-}

import Dict exposing (Dict)
import Map exposing (Alter, Map)


{-| Map each value inside a `Dict`

    import Map
    import Dict.Map
    import Record
    import Dict exposing (Dict)

    variables : { pool : Dict String { value : Int } }
    variables =
        { pool =
            Dict.fromList
                [ ( "a", { value = 2 } )
                , ( "b", { value = 3 } )
                , ( "c", { value = 4 } )
                ]
        }

    variables
        |> Map.over (Record.pool << Dict.Map.valueEach << Record.value) (\n -> n * 10)
    --> { pool =
    -->     Dict.fromList
    -->         [ ( "a", { value = 20 } ), ( "b", { value = 30 } ), ( "c", { value = 40 } ) ]
    --> }

    variables
        |> Map.over (Record.pool << Dict.Map.valueEach << Record.value) (\n -> n + 1)
    --> { pool =
    -->     Dict.fromList
    -->         [ ( "a", { value = 3 } ), ( "b", { value = 4 } ), ( "c", { value = 5 } ) ]
    --> }

-}
valueEach : Map (Dict key value) value (Dict key valueMapped) valueMapped
valueEach =
    Map.at "value each"
        (\valueMap -> Dict.map (\_ -> valueMap))


{-| Map a `Dict`'s value at a given key

    import Dict exposing (Dict)
    import Map exposing (onJust)
    import Dict.Map
    import Record

    dict : Dict Char { value : Int }
    dict =
        Dict.fromList [ ( 'b', { value = 2 } ) ]

    dict
        |> Map.over
            (Dict.Map.valueAt ( 'x', String.fromChar ) << Record.value)
            (\_ -> 3)
    --> dict

[`valueAtString`](#valueAtString) is short for `Dict.Map.valueAt ( stringKey, identity )`.

-}
valueAt :
    ( comparableKey, comparableKey -> String )
    -> Alter (Dict comparableKey value) value
valueAt ( key, keyToString ) =
    Map.at (key |> keyToString)
        (\valueMap structure ->
            structure |> Dict.update key (Maybe.map valueMap)
        )


{-| Shorthand for [`Dict.Map.valueAt ( "key String", identity )`](#valueAt)
-}
valueAtString : String -> Alter (Dict String value) value
valueAtString key =
    valueAt ( key, identity )
