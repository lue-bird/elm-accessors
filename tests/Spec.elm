module Spec exposing (suite)

import Accessor exposing (Relation, map, mapLazy, onJust, valueElseOnNothing, view)
import Dict exposing (Dict)
import Dict.Accessor as Dict
import Expect
import Field
import List.Accessor as List
import Test exposing (Test, test)


simpleRecord : { foo : number, bar : String, qux : Bool }
simpleRecord =
    { foo = 3, bar = "Yop", qux = False }


anotherRecord : { foo : number, bar : String, qux : Bool }
anotherRecord =
    { foo = 5, bar = "Sup", qux = True }


nestedRecord : { foo : { foo : number, bar : String, qux : Bool } }
nestedRecord =
    { foo = simpleRecord }


recordWithList : { bar : List { foo : number, bar : String, qux : Bool } }
recordWithList =
    { bar = [ simpleRecord, anotherRecord ] }


maybeRecord : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
maybeRecord =
    { bar = Just simpleRecord, foo = Nothing }


dict : Dict String number
dict =
    Dict.fromList [ ( "foo", 7 ) ]


recordWithDict : { bar : Dict String number }
recordWithDict =
    { bar = dict }


dictWithRecord : Dict String { bar : String }
dictWithRecord =
    Dict.fromList [ ( "foo", { bar = "Yop" } ) ]


suite : Test
suite =
    Test.describe
        "strict lenses"
        [ Test.describe
            "view"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> view Field.foo
                        |> Expect.equal 3
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> view (Field.foo << Field.bar)
                        |> Expect.equal "Yop"
            , test "in list" <|
                \_ ->
                    recordWithList
                        |> view (Field.bar << List.elementEach << Field.foo)
                        |> Expect.equal [ 3, 5 ]
            , test "in Just" <|
                \_ ->
                    maybeRecord
                        |> view (Field.bar << onJust << Field.qux)
                        |> Expect.equal (Just False)
            , test "in Nothing" <|
                \_ ->
                    maybeRecord
                        |> view (Field.foo << onJust << Field.bar)
                        |> Expect.equal Nothing
            , Test.describe
                "dict"
                [ test "present" <|
                    \_ ->
                        dict
                            |> view (Dict.valueAtString "foo")
                            |> Expect.equal (Just 7)
                , test "absent" <|
                    \_ ->
                        dict
                            |> view (Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "nested present" <|
                    \_ ->
                        recordWithDict
                            |> view (Field.bar << Dict.valueAtString "foo")
                            |> Expect.equal (Just 7)
                , test "nested absent" <|
                    \_ ->
                        recordWithDict
                            |> view (Field.bar << Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "with try" <|
                    \_ ->
                        dictWithRecord
                            |> view (Dict.valueAtString "foo" << onJust << Field.bar)
                            |> Expect.equal (Just "Yop")
                , test "with valueElseOnNothing" <|
                    \_ ->
                        dictWithRecord
                            |> view (Dict.valueAtString "not_it" << valueElseOnNothing { bar = "Stuff" } << Field.bar)
                            |> Expect.equal "Stuff"
                ]
            ]
        , Test.describe
            "map (\\_ -> ...)"
            [ test "simple" <|
                \_ ->
                    let
                        updatedExample : { foo : number, bar : String, qux : Bool }
                        updatedExample =
                            simpleRecord |> map Field.qux (\_ -> True)
                    in
                    updatedExample.qux
                        |> Expect.equal True
            , test "nested" <|
                \_ ->
                    let
                        updatedExample : { foo : { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            nestedRecord |> map (Field.foo << Field.foo) (\_ -> 5)
                    in
                    updatedExample.foo.foo
                        |> Expect.equal 5
            , test "in list" <|
                \_ ->
                    let
                        updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            recordWithList |> map (Field.bar << List.elementEach << Field.bar) (\_ -> "Why, hello")
                    in
                    updatedExample
                        |> view (Field.bar << List.elementEach << Field.bar)
                        |> Expect.equal [ "Why, hello", "Why, hello" ]
            , test "in Just" <|
                \_ ->
                    let
                        updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            maybeRecord |> map (Field.bar << onJust << Field.foo) (\_ -> 4)
                    in
                    updatedExample
                        |> view (Field.bar << onJust << Field.foo)
                        |> Expect.equal (Just 4)
            , test "in Nothing" <|
                \_ ->
                    let
                        -- updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            maybeRecord |> map (Field.foo << onJust << Field.bar) (\_ -> "Nope")
                    in
                    updatedExample
                        |> view (Field.foo << onJust << Field.bar)
                        |> Expect.equal Nothing
            , Test.describe
                "dict"
                [ test "set currently present to present" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                dict |> map (Dict.valueAtString "foo") (\_ -> Just 9)
                        in
                        updatedDict
                            |> view (Dict.valueAtString "foo")
                            |> Expect.equal (Just 9)
                , test "set currently absent to present" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                dict |> map (Dict.valueAtString "bar") (\_ -> Just 9)
                        in
                        updatedDict
                            |> view (Dict.valueAtString "bar")
                            |> Expect.equal (Just 9)
                , test "set currently present to absent" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                dict |> map (Dict.valueAtString "foo") (\_ -> Nothing)
                        in
                        updatedDict
                            |> view (Dict.valueAtString "foo")
                            |> Expect.equal Nothing
                , test "set currently absent to absent" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                dict |> map (Dict.valueAtString "bar") (\_ -> Nothing)
                        in
                        updatedDict |> view (Dict.valueAtString "bar") |> Expect.equal Nothing
                , test "set with try present" <|
                    \_ ->
                        let
                            updatedDict : Dict String { bar : String }
                            updatedDict =
                                dictWithRecord |> map (Dict.valueAtString "foo" << onJust << Field.bar) (\_ -> "Sup")
                        in
                        updatedDict
                            |> view (Dict.valueAtString "foo" << onJust << Field.bar)
                            |> Expect.equal (Just "Sup")
                , test "set with try absent" <|
                    \_ ->
                        let
                            updatedDict : Dict String { bar : String }
                            updatedDict =
                                dictWithRecord |> map (Dict.valueAtString "bar" << onJust << Field.bar) (\_ -> "Sup")
                        in
                        updatedDict
                            |> view (Dict.valueAtString "bar" << onJust << Field.bar)
                            |> Expect.equal Nothing
                ]
            ]
        , Test.describe
            "map"
            [ test "simple" <|
                \_ ->
                    let
                        updatedExample : { foo : number, bar : String, qux : Bool }
                        updatedExample =
                            simpleRecord |> map Field.bar (\w -> w ++ " lait")
                    in
                    updatedExample.bar
                        |> Expect.equal "Yop lait"
            , test "nested" <|
                \_ ->
                    let
                        updatedExample : { foo : { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            nestedRecord |> map (Field.foo << Field.qux) (\w -> not w)
                    in
                    updatedExample.foo.qux
                        |> Expect.equal True
            , test "list" <|
                \_ ->
                    let
                        updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            map (Field.bar << List.elementEach << Field.foo) (\n -> n - 2) recordWithList
                    in
                    updatedExample
                        |> view (Field.bar << List.elementEach << Field.foo)
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    let
                        updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            maybeRecord |> map (Field.bar << onJust << Field.foo) (\n -> n + 3)
                    in
                    updatedExample
                        |> view (Field.bar << onJust << Field.foo)
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    let
                        -- updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            maybeRecord |> map (Field.foo << onJust << Field.bar) (\w -> w ++ "!")
                    in
                    updatedExample
                        |> view (Field.foo << onJust << Field.bar)
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "mapLazy"
            [ test "simple" <|
                \_ ->
                    let
                        updatedExample =
                            simpleRecord |> mapLazy Field.bar (\w -> w ++ " lait")
                    in
                    updatedExample.bar
                        |> Expect.equal "Yop lait"
            , test "nested" <|
                \_ ->
                    let
                        updatedExample =
                            mapLazy (Field.foo << Field.qux) (\w -> not w) nestedRecord
                    in
                    updatedExample.foo.qux
                        |> Expect.equal True
            , test "list" <|
                \_ ->
                    let
                        updatedExample =
                            mapLazy (Field.bar << List.elementEach << Field.foo) (\n -> n - 2) recordWithList
                    in
                    updatedExample
                        |> view (Field.bar << List.elementEach << Field.foo)
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    let
                        updatedExample =
                            maybeRecord |> mapLazy (Field.bar << onJust << Field.foo) (\n -> n + 3)
                    in
                    updatedExample
                        |> view (Field.bar << onJust << Field.foo)
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    let
                        updatedExample =
                            maybeRecord |> mapLazy (Field.foo << onJust << Field.bar) (\w -> w ++ "!")
                    in
                    updatedExample
                        |> view (Field.foo << onJust << Field.bar)
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "making accessors"
            [ let
                myFoo =
                    Accessor.for1To1
                        { description = { structure = "record", focus = ".foo" }
                        , view = .foo
                        , map = \alter record -> { record | foo = alter record.foo }
                        }
              in
              Test.describe
                "Accessor.for1To1"
                [ test "view" <|
                    \_ ->
                        nestedRecord
                            |> view (myFoo << Field.bar)
                            |> Expect.equal "Yop"
                , test "set" <|
                    \_ ->
                        let
                            updatedRec : { foo : { foo : number, bar : String, qux : Bool } }
                            updatedRec =
                                nestedRecord |> map (Field.foo << myFoo) (\_ -> 1)
                        in
                        updatedRec.foo.foo
                            |> Expect.equal 1
                , test "map" <|
                    \_ ->
                        let
                            updatedRec : { foo : { foo : number, bar : String, qux : Bool } }
                            updatedRec =
                                map (myFoo << myFoo) (\n -> n + 3) nestedRecord
                        in
                        updatedRec.foo.foo
                            |> Expect.equal 6
                ]
            , let
                myOnEach =
                    Accessor.for1ToN
                        { description = { structure = "List", focus = "element List.elementEach" }
                        , view = List.map
                        , map = List.map
                        }
              in
              Test.describe
                "Accessor."
                [ test "view" <|
                    \_ ->
                        recordWithList
                            |> view (Field.bar << myOnEach << Field.foo)
                            |> Expect.equal [ 3, 5 ]
                , test "set" <|
                    \_ ->
                        let
                            updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                            updatedExample =
                                recordWithList
                                    |> map (Field.bar << myOnEach << Field.bar) (\_ -> "Greetings")
                        in
                        updatedExample
                            |> view (Field.bar << List.elementEach << Field.bar)
                            |> Expect.equal [ "Greetings", "Greetings" ]
                , test "map" <|
                    \_ ->
                        let
                            updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                            updatedExample =
                                map (Field.bar << myOnEach << Field.foo) (\n -> n - 2) recordWithList
                        in
                        updatedExample
                            |> view (Field.bar << List.elementEach << Field.foo)
                            |> Expect.equal [ 1, 3 ]
                ]
            ]
        ]
