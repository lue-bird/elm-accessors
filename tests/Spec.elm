module Spec exposing (suite)

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


suite : Test
suite =
    Test.describe
        "strict lenses"
        [ Test.describe
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
                        |> .bar
                        |> List.map .bar
                        |> Expect.equal [ "Why, hello", "Why, hello" ]
            , test "in Just" <|
                \_ ->
                    maybeRecord
                        |> Map.over
                            (Record.bar << onJust << Record.foo)
                            (\_ -> 4)
                        |> .bar
                        |> Maybe.map .foo
                        |> Expect.equal (Just 4)
            , test "in Nothing" <|
                \_ ->
                    maybeRecord
                        |> Map.over
                            (Record.foo << onJust << Record.bar)
                            (\_ -> "Nope")
                        |> .foo
                        |> Maybe.map .bar
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
                        |> .bar
                        |> List.map .foo
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    maybeRecord
                        |> Map.over (Record.bar << onJust << Record.foo) (\n -> n + 3)
                        |> .bar
                        |> Maybe.map .foo
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    maybeRecord
                        |> Map.over (Record.foo << onJust << Record.bar) (\w -> w ++ "!")
                        |> .foo
                        |> Maybe.map .bar
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
                        |> .bar
                        |> List.map .foo
                        |> Expect.equal [ 1, 3 ]
            , test "through Just" <|
                \_ ->
                    maybeRecord
                        |> Map.overLazy (Record.bar << onJust << Record.foo) (\n -> n + 3)
                        |> .bar
                        |> Maybe.map .foo
                        |> Expect.equal (Just 6)
            , test "through Nothing" <|
                \_ ->
                    maybeRecord
                        |> Map.overLazy (Record.foo << onJust << Record.bar) (\w -> w ++ "!")
                        |> .foo
                        |> Maybe.map .bar
                        |> Expect.equal Nothing
            ]
        , Test.describe
            "create"
            [ let
                myRecordFoo =
                    Map.at "foo"
                        (\alter record -> { record | foo = alter record.foo })
              in
              Test.describe
                "part"
                [ test "set" <|
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
                myEach =
                    Map.at "each" List.map
              in
              Test.describe
                "each"
                [ test "set" <|
                    \_ ->
                        recordWithList
                            |> Map.over (Record.bar << myEach << Record.bar) (\_ -> "Greetings")
                            |> .bar
                            |> List.map .bar
                            |> Expect.equal [ "Greetings", "Greetings" ]
                , test "map" <|
                    \_ ->
                        recordWithList
                            |> Map.over (Record.bar << myEach << Record.foo) (\n -> n - 2)
                            |> .bar
                            |> List.map .foo
                            |> Expect.equal [ 1, 3 ]
                ]
            ]
        ]
