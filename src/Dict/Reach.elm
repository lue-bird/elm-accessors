module Dict.Reach exposing (valueEach, valueAt, valueAtString)

{-| Reach into a `Dict`

@docs valueEach, valueAt, valueAtString

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
        |> Reach.view (Record.foo |> Reach.into Dict.Reach.valueEach)
    --> Dict.fromList
    -->     [ ( "a", { bar = 2 } ), ( "b", { bar = 3 } ), ( "c", { bar = 4 } ) ]

    recordDictStringBar
        |> Reach.mapOver (Record.foo |> Reach.into Dict.Reach.valueEach |> Reach.into Record.bar) ((*) 10)
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 20 } ), ( "b", { bar = 30 } ), ( "c", { bar = 40 } ) ]
    --> }

    recordDictStringBar
        |> Reach.view (Record.foo |> Reach.into Dict.Reach.valueEach |> Reach.into Record.bar)
    --> Dict.fromList [ ( "a", 2 ), ( "b", 3 ), ( "c", 4 ) ]

    recordDictStringBar
        |> Reach.mapOver (Record.foo |> Reach.into Dict.Reach.valueEach |> Reach.into Record.bar) (\n -> n + 1)
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


{-| Reach into a `Dict`'s value at a given key

    import Dict exposing (Dict)
    import Reach exposing (onJust)
    import Dict.Reach
    import Record

    dict : Dict Char { bar : Int }
    dict =
        Dict.fromList [ ( 'b', { bar = 2 } ) ]

    dict |> Reach.view (Dict.Reach.valueAt ( 'b', String.fromChar ))
    --> Just { bar = 2 }

    dict |> Reach.view (Dict.Reach.valueAt ( 'a', String.fromChar ))
    --> Nothing

    dict
        |> Reach.view
            (Dict.Reach.valueAt ( 'b', String.fromChar ) |> Reach.into Record.bar)
    --> Just 2

    dict
        |> Reach.mapOver
            (Dict.Reach.valueAt ( 'x', String.fromChar ) |> Reach.into Record.bar)
            (\_ -> 3)
    --> dict

[`valueAtString`](#valueAtString) is short for `Dict.Reach.valueAt ( stringKey, identity )`.

-}
valueAt :
    ( comparableKey, comparableKey -> String )
    ->
        Reach.Maybe
            (Dict comparableKey value)
            value
            valueView
            (Dict comparableKey value)
            value
valueAt ( key, keyToString ) =
    Reach.maybe ("value at " ++ (key |> keyToString))
        { access = Dict.get key
        , map =
            \valueMap structure ->
                structure |> Dict.update key (Maybe.map valueMap)
        }


{-| Shorthand for [`Dict.Reach.valueAt ( "key String", identity )`](#valueAt).
-}
valueAtString :
    String
    ->
        Reach.Maybe
            (Dict String value)
            value
            valueView
            (Dict String value)
            value
valueAtString key =
    valueAt ( key, identity )
