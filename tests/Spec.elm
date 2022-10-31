module Spec exposing (suite)

import Dict exposing (Dict)
import Dict.Map as Dict
import Expect
import List.Map as List
import Map exposing (onJust)
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
                        |> Map.view Record.foo
                        |> Expect.equal 3
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> Map.view (Record.foo << Record.bar)
                        |> Expect.equal "Yop"
            , test "in list" <|
                \_ ->
                    recordWithList
                        |> Map.view (Record.bar << List.each << Record.foo)
                        |> Expect.equal [ 3, 5 ]
            , test "in Just" <|
                \_ ->
                    maybeRecord
                        |> Map.view (Record.bar << onJust << Record.qux)
                        |> Expect.equal (Just False)
            , test "in Nothing" <|
                \_ ->
                    maybeRecord
                        |> Map.view (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            , Test.describe
                "dict"
                [ test "present" <|
                    \_ ->
                        dict
                            |> Map.view (Dict.valueAtString "foo")
                            |> Expect.equal (Just 7)
                , test "absent" <|
                    \_ ->
                        dict
                            |> Map.view (Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "nested present" <|
                    \_ ->
                        recordWithDict
                            |> Map.view (Record.bar << Dict.valueAtString "foo")
                            |> Expect.equal (Just 7)
                , test "nested absent" <|
                    \_ ->
                        recordWithDict
                            |> Map.view (Record.bar << Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "with lens afterwards" <|
                    \_ ->
                        dictWithRecord
                            |> Map.view (Dict.valueAtString "foo" << Record.bar)
                            |> Expect.equal (Just "Yop")
                ]
            ]
        , Test.describe
            "over (\\_ -> ...)"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> Map.over Record.qux (\_ -> True)
                        |> .qux
                        |> Expect.equal True
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> Map.over (Record.foo << Record.foo) (\_ -> 5)
                        |> .foo
                        |> .foo
                        |> Expect.equal 5
            , test "in list" <|
                \_ ->
                    recordWithList
                        |> Map.over
                            (Record.bar << List.each << Record.bar)
                            (\_ -> "Why, hello")
                        |> Map.view (Record.bar << List.each << Record.bar)
                        |> Expect.equal [ "Why, hello", "Why, hello" ]
            , test "in Just" <|
                \_ ->
                    maybeRecord
                        |> Map.over
                            (Record.bar << onJust << Record.foo)
                            (\_ -> 4)
                        |> Map.view (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 4)
            , test "in Nothing" <|
                \_ ->
                    maybeRecord
                        |> Map.over
                            (Record.foo << onJust << Record.bar)
                            (\_ -> "Nope")
                        |> Map.view (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "map"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> Map.over Record.bar (\w -> w ++ " lait")
                        |> .bar
                        |> Expect.equal "Yop lait"
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> Map.over (Record.foo << Record.qux) not
                        |> .foo
                        |> .qux
                        |> Expect.equal True
            , test "list" <|
                \_ ->
                    recordWithList
                        |> Map.over
                            (Record.bar << List.each << Record.foo)
                            (\n -> n - 2)
                        |> Map.view (Record.bar << List.each << Record.foo)
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    maybeRecord
                        |> Map.over (Record.bar << onJust << Record.foo) (\n -> n + 3)
                        |> Map.view (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    maybeRecord
                        |> Map.over (Record.foo << onJust << Record.bar) (\w -> w ++ "!")
                        |> Map.view (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "overLazy"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> Map.overLazy Record.bar (\w -> w ++ " lait")
                        |> .bar
                        |> Expect.equal "Yop lait"
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> Map.overLazy (Record.foo << Record.qux) not
                        |> .foo
                        |> .qux
                        |> Expect.equal True
            , test "list" <|
                \_ ->
                    recordWithList
                        |> Map.overLazy
                            (Record.bar << List.each << Record.foo)
                            (\n -> n - 2)
                        |> Map.view (Record.bar << List.each << Record.foo)
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    maybeRecord
                        |> Map.overLazy (Record.bar << onJust << Record.foo) (\n -> n + 3)
                        |> Map.view (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    maybeRecord
                        |> Map.overLazy (Record.foo << onJust << Record.bar) (\w -> w ++ "!")
                        |> Map.view (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "create"
            [ let
                myRecordFoo =
                    Map.at "foo"
                        { access = .foo
                        , map = \alter record -> { record | foo = alter record.foo }
                        }
              in
              Test.describe
                "part"
                [ test "view" <|
                    \_ ->
                        nestedRecord
                            |> Map.view (myRecordFoo << Record.bar)
                            |> Expect.equal "Yop"
                , test "set" <|
                    \_ ->
                        nestedRecord
                            |> Map.over (Record.foo << myRecordFoo) (\_ -> 1)
                            |> .foo
                            |> .foo
                            |> Expect.equal 1
                , test "map" <|
                    \_ ->
                        nestedRecord
                            |> Map.over (myRecordFoo << myRecordFoo) (\n -> n + 3)
                            |> .foo
                            |> .foo
                            |> Expect.equal 6
                ]
            , let
                myOnEach =
                    Map.elements "element each"
                        { view = List.Map
                        , map = List.Map
                        }
              in
              Test.describe
                "reach"
                [ test "view" <|
                    \_ ->
                        recordWithList
                            |> Map.view (Record.bar << myOnEach << Record.foo)
                            |> Expect.equal [ 3, 5 ]
                , test "set" <|
                    \_ ->
                        recordWithList
                            |> Map.over (Record.bar << myOnEach << Record.bar) (\_ -> "Greetings")
                            |> Map.view (Record.bar << List.each << Record.bar)
                            |> Expect.equal [ "Greetings", "Greetings" ]
                , test "map" <|
                    \_ ->
                        recordWithList
                            |> Map.over (Record.bar << myOnEach << Record.foo) (\n -> n - 2)
                            |> Map.view (Record.bar << List.each << Record.foo)
                            |> Expect.equal [ 1, 3 ]
                ]
            ]
        ]
