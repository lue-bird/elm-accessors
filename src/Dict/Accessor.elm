module Dict.Accessor exposing (valueEach, valueKeyEach, valueAt, valueAtString)

{-| Accessors for `Dict`s.

@docs valueEach, valueKeyEach, valueAt, valueAtString

-}

import Accessor exposing (Lens, Traversal, traversal)
import Dict exposing (Dict)


{-| Traverse each value in a `Dict`.

    import Accessor exposing (mapOver, view)
    import Record
    import List.Accessor
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
        |> view (Record.foo << Dict.Accessor.valueEach)
    --> Dict.fromList
    -->     [ ( "a", { bar = 2 } ), ( "b", { bar = 3 } ), ( "c", { bar = 4 } ) ]

    recordDictStringBar
        |> mapOver
            (Record.foo << Dict.Accessor.valueEach << Record.bar)
            ((*) 10)
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "a", { bar = 20 } ), ( "b", { bar = 30 } ), ( "c", { bar = 40 } ) ]
    --> }

    recordDictStringBar
        |> view (Record.foo << Dict.Accessor.valueEach << Record.bar)
    --> Dict.fromList [ ( "a", 2 ), ( "b", 3 ), ( "c", 4 ) ]

    recordDictStringBar
        |> mapOver (Record.foo << Dict.Accessor.valueEach << Record.bar) ((+) 1)
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

    import Accessor exposing (view, mapOver)
    import Dict.Accessor
    import Record
    import Dict exposing (Dict)

    recordDictStringBar : { foo : Dict String { bar : Int } }
    recordDictStringBar =
        { foo =
            Dict.fromList
                [ ( "up", { bar = 2 } )
                , ( "woo", { bar = 3 } )
                , ( "yoink", { bar = 4 } )
                ]
        }

    recordDictStringBar
        |> view (Record.foo << Dict.Accessor.valueKeyEach)
    --> Dict.fromList
    -->     [ ( "up", { key = "up", value = { bar = 2 } } )
    -->     , ( "woo", { key = "woo", value = { bar = 3 } } )
    -->     , ( "yoink", { key = "yoink", value = { bar = 4 } } )
    -->     ]


    upMultiplyBy10 : { key : String, value : { bar : Int } } -> { key : String, value : { bar : Int } }
    upMultiplyBy10 =
        \entry ->
            { entry
                | value =
                    if entry.key |> String.startsWith "up" then
                        { bar = entry.value.bar * 10 }
                    else
                        entry.value
            }

    recordDictStringBar
        |> mapOver
            (Record.foo << Dict.Accessor.valueKeyEach)
            upMultiplyBy10
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "up", { bar = 20 } ), ( "woo", { bar = 3 } ), ( "yoink", { bar = 4 } ) ]
    --> }

    recordDictStringBar
        |> view (Record.foo << Dict.Accessor.valueKeyEach << Record.value << Record.bar)
    --> Dict.fromList [ ( "up", 2 ), ( "woo", 3 ), ( "yoink", 4 ) ]

    recordDictStringBar
        |> mapOver
            (Record.foo << Dict.Accessor.valueKeyEach << Record.value << Record.bar)
            ((+) 1)
    --> { foo =
    -->     Dict.fromList
    -->         [ ( "up", { bar = 3 } ), ( "woo", { bar = 4 } ), ( "yoink", { bar = 5 } ) ]
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
    import Accessor exposing (view, mapOver, onJust)
    import Dict.Accessor
    import Record

    dict : Dict Char { bar : Int }
    dict =
        Dict.fromList [ ( 'b', { bar = 2 } ) ]

    dict
        |> view (Dict.Accessor.valueAt ( 'b', String.fromChar ))
    --> Just { bar = 2 }

    dict
        |> view (Dict.Accessor.valueAt ( 'x', String.fromChar ))
    --> Nothing

    dict
        |> view (Dict.Accessor.valueAt ( 'b', String.fromChar ) << onJust << Record.bar)
    --> Just 2

    dict
        |> mapOver
            (Dict.Accessor.valueAt ( 'b', String.fromChar ))
            (\_ -> Nothing)
    --> dict |> Dict.remove 'b'

    dict
        |> mapOver
            (Dict.Accessor.valueAt ( 'x', String.fromChar ) << onJust << Record.bar)
            (\_ -> 3)
    --> dict

[`valueAtString`](#valueAtString) is a shorthand for `Dict.Accessor.valueAt ( stringKey, \keyString -> "\"" ++ keyString ++ "\"" )`.

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


{-| Shorthand for [`Dict.Accessor.valueAt ( "key String", \keyString -> "\"" ++ keyString ++ "\"" )`](#valueAt).
-}
valueAtString :
    String
    -> Lens (Dict String value) (Maybe value) valueFocus valueFocusView
valueAtString key =
    valueAt ( key, \keyString -> "\"" ++ keyString ++ "\"" )
