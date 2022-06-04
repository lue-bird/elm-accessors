module Spec exposing (suite)

import Accessor exposing (mapOver, mapOverLazy, onJust, valueElseOnNothing, view)
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
                        |> view Record.foo
                        |> Expect.equal 3
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> view (Record.foo << Record.bar)
                        |> Expect.equal "Yop"
            , test "in list" <|
                \_ ->
                    recordWithList
                        |> view (Record.bar << List.elementEach << Record.foo)
                        |> Expect.equal [ 3, 5 ]
            , test "in Just" <|
                \_ ->
                    maybeRecord
                        |> view (Record.bar << onJust << Record.qux)
                        |> Expect.equal (Just False)
            , test "in Nothing" <|
                \_ ->
                    maybeRecord
                        |> view (Record.foo << onJust << Record.bar)
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
                            |> view (Record.bar << Dict.valueAtString "foo")
                            |> Expect.equal (Just 7)
                , test "nested absent" <|
                    \_ ->
                        recordWithDict
                            |> view (Record.bar << Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "with try" <|
                    \_ ->
                        dictWithRecord
                            |> view (Dict.valueAtString "foo" << onJust << Record.bar)
                            |> Expect.equal (Just "Yop")
                , test "with valueElseOnNothing" <|
                    \_ ->
                        dictWithRecord
                            |> view
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
                        |> mapOver Record.qux (\_ -> True)
                        |> .qux
                        |> Expect.equal True
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> mapOver (Record.foo << Record.foo) (\_ -> 5)
                        |> .foo
                        |> .foo
                        |> Expect.equal 5
            , test "in list" <|
                \_ ->
                    recordWithList
                        |> mapOver
                            (Record.bar << List.elementEach << Record.bar)
                            (\_ -> "Why, hello")
                        |> view (Record.bar << List.elementEach << Record.bar)
                        |> Expect.equal [ "Why, hello", "Why, hello" ]
            , test "in Just" <|
                \_ ->
                    maybeRecord
                        |> mapOver
                            (Record.bar << onJust << Record.foo)
                            (\_ -> 4)
                        |> view (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 4)
            , test "in Nothing" <|
                \_ ->
                    maybeRecord
                        |> mapOver
                            (Record.foo << onJust << Record.bar)
                            (\_ -> "Nope")
                        |> view (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            , Test.describe
                "dict"
                [ test "set currently present to present" <|
                    \_ ->
                        dict
                            |> mapOver (Dict.valueAtString "foo") (\_ -> Just 9)
                            |> view (Dict.valueAtString "foo")
                            |> Expect.equal (Just 9)
                , test "set currently absent to present" <|
                    \_ ->
                        dict
                            |> mapOver (Dict.valueAtString "bar") (\_ -> Just 9)
                            |> view (Dict.valueAtString "bar")
                            |> Expect.equal (Just 9)
                , test "set currently present to absent" <|
                    \_ ->
                        dict
                            |> mapOver (Dict.valueAtString "foo") (\_ -> Nothing)
                            |> view (Dict.valueAtString "foo")
                            |> Expect.equal Nothing
                , test "set currently absent to absent" <|
                    \_ ->
                        dict
                            |> mapOver (Dict.valueAtString "bar") (\_ -> Nothing)
                            |> view (Dict.valueAtString "bar")
                            |> Expect.equal Nothing
                , test "set with try present" <|
                    \_ ->
                        dictWithRecord
                            |> mapOver
                                (Dict.valueAtString "foo" << onJust << Record.bar)
                                (\_ -> "Sup")
                            |> view (Dict.valueAtString "foo" << onJust << Record.bar)
                            |> Expect.equal (Just "Sup")
                , test "set with try absent" <|
                    \_ ->
                        dictWithRecord
                            |> mapOver
                                (Dict.valueAtString "bar" << onJust << Record.bar)
                                (\_ -> "Sup")
                            |> view (Dict.valueAtString "bar" << onJust << Record.bar)
                            |> Expect.equal Nothing
                ]
            ]
        , Test.describe
            "map"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> mapOver Record.bar (\w -> w ++ " lait")
                        |> .bar
                        |> Expect.equal "Yop lait"
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> mapOver (Record.foo << Record.qux) not
                        |> .foo
                        |> .qux
                        |> Expect.equal True
            , test "list" <|
                \_ ->
                    recordWithList
                        |> mapOver
                            (Record.bar << List.elementEach << Record.foo)
                            (\n -> n - 2)
                        |> view (Record.bar << List.elementEach << Record.foo)
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    maybeRecord
                        |> mapOver (Record.bar << onJust << Record.foo) (\n -> n + 3)
                        |> view (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    maybeRecord
                        |> mapOver (Record.foo << onJust << Record.bar) (\w -> w ++ "!")
                        |> view (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "mapLazy"
            [ test "simple" <|
                \_ ->
                    simpleRecord
                        |> mapOverLazy Record.bar (\w -> w ++ " lait")
                        |> .bar
                        |> Expect.equal "Yop lait"
            , test "nested" <|
                \_ ->
                    nestedRecord
                        |> mapOverLazy (Record.foo << Record.qux) not
                        |> .foo
                        |> .qux
                        |> Expect.equal True
            , test "list" <|
                \_ ->
                    recordWithList
                        |> mapOverLazy
                            (Record.bar << List.elementEach << Record.foo)
                            (\n -> n - 2)
                        |> view (Record.bar << List.elementEach << Record.foo)
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    maybeRecord
                        |> mapOverLazy (Record.bar << onJust << Record.foo) (\n -> n + 3)
                        |> view (Record.bar << onJust << Record.foo)
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    maybeRecord
                        |> mapOverLazy (Record.foo << onJust << Record.bar) (\w -> w ++ "!")
                        |> view (Record.foo << onJust << Record.bar)
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "making accessors"
            [ let
                myRecordFoo =
                    Accessor.lens
                        { description = { structure = "record", focus = ".foo" }
                        , view = .foo
                        , map = \alter record -> { record | foo = alter record.foo }
                        , focusName =
                            \focusFocusNamed -> { foo = focusFocusNamed }
                        }
              in
              Test.describe
                "Accessor.lens"
                [ test "view" <|
                    \_ ->
                        nestedRecord
                            |> view (myRecordFoo << Record.bar)
                            |> Expect.equal "Yop"
                , test "set" <|
                    \_ ->
                        nestedRecord
                            |> mapOver (Record.foo << myRecordFoo) (\_ -> 1)
                            |> .foo
                            |> .foo
                            |> Expect.equal 1
                , test "map" <|
                    \_ ->
                        nestedRecord
                            |> mapOver (myRecordFoo << myRecordFoo) (\n -> n + 3)
                            |> .foo
                            |> .foo
                            |> Expect.equal 6
                ]
            , let
                myOnEach =
                    Accessor.traversal
                        { description = { structure = "List", focus = "element List.elementEach" }
                        , view = List.map
                        , map = List.map
                        , focusName = identity
                        }
              in
              Test.describe
                "Accessor"
                [ test "view" <|
                    \_ ->
                        recordWithList
                            |> view (Record.bar << myOnEach << Record.foo)
                            |> Expect.equal [ 3, 5 ]
                , test "set" <|
                    \_ ->
                        recordWithList
                            |> mapOver (Record.bar << myOnEach << Record.bar) (\_ -> "Greetings")
                            |> view (Record.bar << List.elementEach << Record.bar)
                            |> Expect.equal [ "Greetings", "Greetings" ]
                , test "map" <|
                    \_ ->
                        recordWithList
                            |> mapOver (Record.bar << myOnEach << Record.foo) (\n -> n - 2)
                            |> view (Record.bar << List.elementEach << Record.foo)
                            |> Expect.equal [ 1, 3 ]
                ]
            ]
        ]
