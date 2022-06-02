module Array.Accessor exposing (elementEach, elementIndexEach, element)

{-| Accessors for `Array`s.

@docs elementEach, elementIndexEach, element

-}

import Accessor exposing (Accessor, Relation, create1To1, create1ToN, onJust)
import Array exposing (Array)
import Array.Linear
import Linear exposing (DirectionLinear, ExpectedIndexInRange(..))
import Linear.Extra as Linear


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
    import Array.Accessor exposing (element)
    import Record

    barray : Array { bar : String }
    barray =
        Array.fromList [ { bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray |> view (element 1)
    --> Just { bar = "Things" }

    barray |> view (element 9000)
    --> Nothing

    barray |> view (element 0 << Record.bar)
    --> Just "Stuff"

    barray |> mapOver (element 0 << Record.bar) (\_ -> "Whatever")
    --> Array.fromList [ { bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" } ]

    barray |> mapOver (element 9000 << Record.bar) (\_ -> "Whatever")
    --> barray

-}
element :
    ( DirectionLinear, Int )
    -> Accessor (Array element) element (Maybe focusFocusView) focusFocus focusFocusView
element location =
    create1To1
        { description =
            { structure = "Array"
            , focus = "element " ++ (location |> Linear.locationToString)
            }
        , view =
            \array ->
                case array |> Array.Linear.element location of
                    Err (ExpectedIndexForLength _) ->
                        Nothing

                    Ok value ->
                        value |> Just
        , map =
            \alter ->
                \array ->
                    -- `<< onJust` at the end ensures we can't delete any existing keys
                    -- so `List.filterMap identity` should be safe
                    array
                        |> Array.map Just
                        |> Array.Linear.elementAlter ( location, alter )
                        |> arrayValues
        }
        << onJust


arrayValues : Array (Maybe element) -> Array element
arrayValues =
    \arrayMaybe ->
        arrayMaybe
            |> Array.toList
            |> List.filterMap identity
            |> Array.fromList
