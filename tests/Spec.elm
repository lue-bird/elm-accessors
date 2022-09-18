module Spec exposing (suite)

import Dict exposing (Dict)
import Dict.Reach as Dict
import Expect
import List.Reach as List
import Reach exposing (onJust, valueElseOnNothing)
import Record
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
                        |> Reach.view Record.foo
                        |> Expect.equal 3
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> Reach.view (Record.foo << Record.bar)
                        |> Expect.equal "Yop"
            , test "in list" <|
                \_ ->
                    recordWithList
                        |> Reach.view (Record.bar << List.elementEach << Record.foo)
                        |> Expect.equal [ 3, 5 ]
            , test "in Just" <|
                \_ ->
                    maybeRecord
                        |> Reach.view (Record.bar << onJust << Record.qux)
                        |> Expect.equal (Just False)
            , test "in Nothing" <|
                \_ ->
                    maybeRecord
                        |> Reach.view (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            , Test.describe
                "dict"
                [ test "present" <|
                    \_ ->
                        dict
                            |> Reach.view (Dict.valueAtString "foo")
                            |> Expect.equal (Just 7)
                , test "absent" <|
                    \_ ->
                        dict
                            |> Reach.view (Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "nested present" <|
                    \_ ->
                        recordWithDict
                            |> Reach.view (Record.bar << Dict.valueAtString "foo")
                            |> Expect.equal (Just 7)
                , test "nested absent" <|
                    \_ ->
                        recordWithDict
                            |> Reach.view (Record.bar << Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "with try" <|
                    \_ ->
                        dictWithRecord
                            |> Reach.view (Dict.valueAtString "foo" << onJust << Record.bar)
                            |> Expect.equal (Just "Yop")
                , test "with valueElseOnNothing" <|
                    \_ ->
                        dictWithRecord
                            |> Reach.view
                                (Dict.valueAtString "not_it"
                                    << valueElseOnNothing { bar = "Stuff" }
                                    << Record.bar
                                )
                            |> Expect.equal "Stuff"
                ]
            ]
        , Test.describe
            "mapOver (\\_ -> ...)"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> Reach.mapOver Record.qux (\_ -> True)
                        |> .qux
                        |> Expect.equal True
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> Reach.mapOver (Record.foo << Record.foo) (\_ -> 5)
                        |> .foo
                        |> .foo
                        |> Expect.equal 5
            , test "in list" <|
                \_ ->
                    recordWithList
                        |> Reach.mapOver
                            (Record.bar << List.elementEach << Record.bar)
                            (\_ -> "Why, hello")
                        |> Reach.view (Record.bar << List.elementEach << Record.bar)
                        |> Expect.equal [ "Why, hello", "Why, hello" ]
            , test "in Just" <|
                \_ ->
                    maybeRecord
                        |> Reach.mapOver
                            (Record.bar << onJust << Record.foo)
                            (\_ -> 4)
                        |> Reach.view (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 4)
            , test "in Nothing" <|
                \_ ->
                    maybeRecord
                        |> Reach.mapOver
                            (Record.foo << onJust << Record.bar)
                            (\_ -> "Nope")
                        |> Reach.view (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            , Test.describe
                "dict"
                [ test "set currently present to present" <|
                    \_ ->
                        dict
                            |> Reach.mapOver (Dict.valueAtString "foo") (\_ -> Just 9)
                            |> Reach.view (Dict.valueAtString "foo")
                            |> Expect.equal (Just 9)
                , test "set currently absent to present" <|
                    \_ ->
                        dict
                            |> Reach.mapOver (Dict.valueAtString "bar") (\_ -> Just 9)
                            |> Reach.view (Dict.valueAtString "bar")
                            |> Expect.equal (Just 9)
                , test "set currently present to absent" <|
                    \_ ->
                        dict
                            |> Reach.mapOver (Dict.valueAtString "foo") (\_ -> Nothing)
                            |> Reach.view (Dict.valueAtString "foo")
                            |> Expect.equal Nothing
                , test "set currently absent to absent" <|
                    \_ ->
                        dict
                            |> Reach.mapOver (Dict.valueAtString "bar") (\_ -> Nothing)
                            |> Reach.view (Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "set with try present" <|
                    \_ ->
                        dictWithRecord
                            |> Reach.mapOver
                                (Dict.valueAtString "foo" << onJust << Record.bar)
                                (\_ -> "Sup")
                            |> Reach.view (Dict.valueAtString "foo" << onJust << Record.bar)
                            |> Expect.equal (Just "Sup")
                , test "set with try absent" <|
                    \_ ->
                        dictWithRecord
                            |> Reach.mapOver
                                (Dict.valueAtString "bar" << onJust << Record.bar)
                                (\_ -> "Sup")
                            |> Reach.view (Dict.valueAtString "bar" << onJust << Record.bar)
                            |> Expect.equal Nothing
                ]
            ]
        , Test.describe
            "map"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> Reach.mapOver Record.bar (\w -> w ++ " lait")
                        |> .bar
                        |> Expect.equal "Yop lait"
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> Reach.mapOver (Record.foo << Record.qux) not
                        |> .foo
                        |> .qux
                        |> Expect.equal True
            , test "list" <|
                \_ ->
                    recordWithList
                        |> Reach.mapOver
                            (Record.bar << List.elementEach << Record.foo)
                            (\n -> n - 2)
                        |> Reach.view (Record.bar << List.elementEach << Record.foo)
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    maybeRecord
                        |> Reach.mapOver (Record.bar << onJust << Record.foo) (\n -> n + 3)
                        |> Reach.view (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    maybeRecord
                        |> Reach.mapOver (Record.foo << onJust << Record.bar) (\w -> w ++ "!")
                        |> Reach.view (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "overLazy"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> Reach.mapOverLazy Record.bar (\w -> w ++ " lait")
                        |> .bar
                        |> Expect.equal "Yop lait"
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> Reach.mapOverLazy (Record.foo << Record.qux) not
                        |> .foo
                        |> .qux
                        |> Expect.equal True
            , test "list" <|
                \_ ->
                    recordWithList
                        |> Reach.mapOverLazy
                            (Record.bar << List.elementEach << Record.foo)
                            (\n -> n - 2)
                        |> Reach.view (Record.bar << List.elementEach << Record.foo)
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    maybeRecord
                        |> Reach.mapOverLazy (Record.bar << onJust << Record.foo) (\n -> n + 3)
                        |> Reach.view (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    maybeRecord
                        |> Reach.mapOverLazy (Record.foo << onJust << Record.bar) (\w -> w ++ "!")
                        |> Reach.view (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "create"
            [ let
                myRecordFoo =
                    Reach.part "foo"
                        { access = .foo
                        , map = \alter record -> { record | foo = alter record.foo }
                        }
              in
              Test.describe
                "part"
                [ test "view" <|
                    \_ ->
                        nestedRecord
                            |> Reach.view (myRecordFoo << Record.bar)
                            |> Expect.equal "Yop"
                , test "set" <|
                    \_ ->
                        nestedRecord
                            |> Reach.mapOver (Record.foo << myRecordFoo) (\_ -> 1)
                            |> .foo
                            |> .foo
                            |> Expect.equal 1
                , test "map" <|
                    \_ ->
                        nestedRecord
                            |> Reach.mapOver (myRecordFoo << myRecordFoo) (\n -> n + 3)
                            |> .foo
                            |> .foo
                            |> Expect.equal 6
                ]
            , let
                myOnEach =
                    Reach.elements "element each"
                        { view = List.map
                        , map = List.map
                        }
              in
              Test.describe
                "reach"
                [ test "view" <|
                    \_ ->
                        recordWithList
                            |> Reach.view (Record.bar << myOnEach << Record.foo)
                            |> Expect.equal [ 3, 5 ]
                , test "set" <|
                    \_ ->
                        recordWithList
                            |> Reach.mapOver (Record.bar << myOnEach << Record.bar) (\_ -> "Greetings")
                            |> Reach.view (Record.bar << List.elementEach << Record.bar)
                            |> Expect.equal [ "Greetings", "Greetings" ]
                , test "map" <|
                    \_ ->
                        recordWithList
                            |> Reach.mapOver (Record.bar << myOnEach << Record.foo) (\n -> n - 2)
                            |> Reach.view (Record.bar << List.elementEach << Record.foo)
                            |> Expect.equal [ 1, 3 ]
                ]
            ]
        ]
