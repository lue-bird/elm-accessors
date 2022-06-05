module Spec exposing (suite)

import Accessor exposing (get, onJust, over, over_, valueElseOnNothing)
import Dict exposing (Dict)
import Dict.Accessor as Dict
import Expect
import List.Accessor as List
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
                        |> get Record.foo
                        |> Expect.equal 3
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> get (Record.foo << Record.bar)
                        |> Expect.equal "Yop"
            , test "in list" <|
                \_ ->
                    recordWithList
                        |> get (Record.bar << List.elementEach << Record.foo)
                        |> Expect.equal [ 3, 5 ]
            , test "in Just" <|
                \_ ->
                    maybeRecord
                        |> get (Record.bar << onJust << Record.qux)
                        |> Expect.equal (Just False)
            , test "in Nothing" <|
                \_ ->
                    maybeRecord
                        |> get (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            , Test.describe
                "dict"
                [ test "present" <|
                    \_ ->
                        dict
                            |> get (Dict.valueAtString "foo")
                            |> Expect.equal (Just 7)
                , test "absent" <|
                    \_ ->
                        dict
                            |> get (Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "nested present" <|
                    \_ ->
                        recordWithDict
                            |> get (Record.bar << Dict.valueAtString "foo")
                            |> Expect.equal (Just 7)
                , test "nested absent" <|
                    \_ ->
                        recordWithDict
                            |> get (Record.bar << Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "with try" <|
                    \_ ->
                        dictWithRecord
                            |> get (Dict.valueAtString "foo" << onJust << Record.bar)
                            |> Expect.equal (Just "Yop")
                , test "with valueElseOnNothing" <|
                    \_ ->
                        dictWithRecord
                            |> get
                                (Dict.valueAtString "not_it"
                                    << valueElseOnNothing { bar = "Stuff" }
                                    << Record.bar
                                )
                            |> Expect.equal "Stuff"
                ]
            ]
        , Test.describe
            "map (\\_ -> ...)"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> over Record.qux (\_ -> True)
                        |> .qux
                        |> Expect.equal True
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> over (Record.foo << Record.foo) (\_ -> 5)
                        |> .foo
                        |> .foo
                        |> Expect.equal 5
            , test "in list" <|
                \_ ->
                    recordWithList
                        |> over
                            (Record.bar << List.elementEach << Record.bar)
                            (\_ -> "Why, hello")
                        |> get (Record.bar << List.elementEach << Record.bar)
                        |> Expect.equal [ "Why, hello", "Why, hello" ]
            , test "in Just" <|
                \_ ->
                    maybeRecord
                        |> over
                            (Record.bar << onJust << Record.foo)
                            (\_ -> 4)
                        |> get (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 4)
            , test "in Nothing" <|
                \_ ->
                    maybeRecord
                        |> over
                            (Record.foo << onJust << Record.bar)
                            (\_ -> "Nope")
                        |> get (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            , Test.describe
                "dict"
                [ test "set currently present to present" <|
                    \_ ->
                        dict
                            |> over (Dict.valueAtString "foo") (\_ -> Just 9)
                            |> get (Dict.valueAtString "foo")
                            |> Expect.equal (Just 9)
                , test "set currently absent to present" <|
                    \_ ->
                        dict
                            |> over (Dict.valueAtString "bar") (\_ -> Just 9)
                            |> get (Dict.valueAtString "bar")
                            |> Expect.equal (Just 9)
                , test "set currently present to absent" <|
                    \_ ->
                        dict
                            |> over (Dict.valueAtString "foo") (\_ -> Nothing)
                            |> get (Dict.valueAtString "foo")
                            |> Expect.equal Nothing
                , test "set currently absent to absent" <|
                    \_ ->
                        dict
                            |> over (Dict.valueAtString "bar") (\_ -> Nothing)
                            |> get (Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "set with try present" <|
                    \_ ->
                        dictWithRecord
                            |> over
                                (Dict.valueAtString "foo" << onJust << Record.bar)
                                (\_ -> "Sup")
                            |> get (Dict.valueAtString "foo" << onJust << Record.bar)
                            |> Expect.equal (Just "Sup")
                , test "set with try absent" <|
                    \_ ->
                        dictWithRecord
                            |> over
                                (Dict.valueAtString "bar" << onJust << Record.bar)
                                (\_ -> "Sup")
                            |> get (Dict.valueAtString "bar" << onJust << Record.bar)
                            |> Expect.equal Nothing
                ]
            ]
        , Test.describe
            "map"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> over Record.bar (\w -> w ++ " lait")
                        |> .bar
                        |> Expect.equal "Yop lait"
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> over (Record.foo << Record.qux) not
                        |> .foo
                        |> .qux
                        |> Expect.equal True
            , test "list" <|
                \_ ->
                    recordWithList
                        |> over
                            (Record.bar << List.elementEach << Record.foo)
                            (\n -> n - 2)
                        |> get (Record.bar << List.elementEach << Record.foo)
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    maybeRecord
                        |> over (Record.bar << onJust << Record.foo) (\n -> n + 3)
                        |> get (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    maybeRecord
                        |> over (Record.foo << onJust << Record.bar) (\w -> w ++ "!")
                        |> get (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "mapLazy"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> over_ Record.bar (\w -> w ++ " lait")
                        |> .bar
                        |> Expect.equal "Yop lait"
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> over_ (Record.foo << Record.qux) not
                        |> .foo
                        |> .qux
                        |> Expect.equal True
            , test "list" <|
                \_ ->
                    recordWithList
                        |> over_
                            (Record.bar << List.elementEach << Record.foo)
                            (\n -> n - 2)
                        |> get (Record.bar << List.elementEach << Record.foo)
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    maybeRecord
                        |> over_ (Record.bar << onJust << Record.foo) (\n -> n + 3)
                        |> get (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    maybeRecord
                        |> over_ (Record.foo << onJust << Record.bar) (\w -> w ++ "!")
                        |> get (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "making accessors"
            [ let
                myRecordFoo =
                    Accessor.lens
                        { name = ".foo"
                        , view = .foo
                        , over = \alter record -> { record | foo = alter record.foo }
                        }
              in
              Test.describe
                "Accessor.lens"
                [ test "view" <|
                    \_ ->
                        nestedRecord
                            |> get (myRecordFoo << Record.bar)
                            |> Expect.equal "Yop"
                , test "set" <|
                    \_ ->
                        nestedRecord
                            |> over (Record.foo << myRecordFoo) (\_ -> 1)
                            |> .foo
                            |> .foo
                            |> Expect.equal 1
                , test "map" <|
                    \_ ->
                        nestedRecord
                            |> over (myRecordFoo << myRecordFoo) (\n -> n + 3)
                            |> .foo
                            |> .foo
                            |> Expect.equal 6
                ]
            , let
                myOnEach =
                    Accessor.traversal
                        { name = "element each"
                        , get = List.map
                        , over = List.map
                        }
              in
              Test.describe
                "Accessor"
                [ test "view" <|
                    \_ ->
                        recordWithList
                            |> get (Record.bar << myOnEach << Record.foo)
                            |> Expect.equal [ 3, 5 ]
                , test "set" <|
                    \_ ->
                        recordWithList
                            |> over (Record.bar << myOnEach << Record.bar) (\_ -> "Greetings")
                            |> get (Record.bar << List.elementEach << Record.bar)
                            |> Expect.equal [ "Greetings", "Greetings" ]
                , test "map" <|
                    \_ ->
                        recordWithList
                            |> over (Record.bar << myOnEach << Record.foo) (\n -> n - 2)
                            |> get (Record.bar << List.elementEach << Record.foo)
                            |> Expect.equal [ 1, 3 ]
                ]
            ]
        ]
