module Laws exposing (tests)

import Array exposing (Array)
import Array.Map
import Dict exposing (Dict)
import Expect
import Fuzz exposing (Fuzzer)
import List.Map
import Map exposing (Alter, onJust)
import Maybe exposing (Maybe)
import Record
import String
import Test exposing (Test, test)


tests : Test
tests =
    Test.describe
        "traversal laws"
        [ settableExamples
        , lensExamples
        , test "description"
            (\() ->
                (Record.info << Record.stuff << List.Map.element 7 << Record.name)
                    |> Map.description
                    |> String.join ")"
                    |> Expect.equal "info)stuff)7)name"
            )
        ]


lensExamples : Test
lensExamples =
    Test.describe
        "lens"
        [ Record.name
            |> isLens
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        , Record.age
            |> isLens
                { structure = personFuzzer
                , focusAlter = intAlter
                , focus = Fuzz.int
                }
        ]


isLens :
    { structure : Fuzzer structure
    , focusAlter : Fuzzer (focus -> focus)
    , focus : Fuzzer focus
    }
    -> Alter structure focus
    -> Test
isLens fuzzer =
    \settable ->
        Test.describe
            (settable |> Map.description |> String.join ")")
            [ isSettable fuzzer settable
            ]


settableExamples : Test
settableExamples =
    let
        check =
            isSettable
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
    in
    Test.describe
        "settable"
        [ check (Record.email << onJust)
        , check (Record.stuff << List.Map.element 0)
        , check (Record.stuff << List.Map.each)
        , check (Record.things << Array.Map.element 0)
        , check (Record.things << Array.Map.each)
        ]


isSettable :
    { structure : Fuzzer structure
    , focusAlter : Fuzzer (focus -> focus)
    , focus : Fuzzer focus
    }
    -> Alter structure focus
    -> Test
isSettable fuzzer settable =
    Test.describe
        ("isSettable "
            ++ (settable |> Map.description |> String.join ")")
        )
        [ setter_identity fuzzer settable
        , setter_composition fuzzer settable
        , setter_set_set fuzzer settable
        ]


setter_identity :
    { m | structure : Fuzzer structure }
    -> Alter structure focus
    -> Test
setter_identity fuzzer settable =
    Test.fuzz
        (Fuzz.constant (\structure -> { structure = structure })
            |> Fuzz.andMap fuzzer.structure
        )
        "setter_identity"
        (\{ structure } ->
            structure
                |> Map.over settable identity
                |> Expect.equal structure
        )


setter_composition :
    { m
        | structure : Fuzzer structure
        , focusAlter : Fuzzer (focus -> focus)
    }
    -> Alter structure focus
    -> Test
setter_composition fuzzer settable =
    Test.fuzz
        (Fuzz.constant
            (\structure alter0 alter1 ->
                { structure = structure
                , alter0 = alter0
                , alter1 = alter1
                }
            )
            |> Fuzz.andMap fuzzer.structure
            |> Fuzz.andMap fuzzer.focusAlter
            |> Fuzz.andMap fuzzer.focusAlter
        )
        "setter_composition"
        (\{ structure, alter0, alter1 } ->
            (structure
                |> Map.over settable alter0
                |> Map.over settable alter1
            )
                |> Expect.equal
                    (structure
                        |> Map.over settable (alter0 >> alter1)
                    )
        )


setter_set_set :
    { m
        | structure : Fuzzer structure
        , focus : Fuzzer focus
    }
    -> Alter structure focus
    -> Test
setter_set_set fuzzer settable =
    Test.fuzz
        (Fuzz.constant
            (\structure focus0 focus1 ->
                { structure = structure
                , focus0 = focus0
                , focus1 = focus1
                }
            )
            |> Fuzz.andMap fuzzer.structure
            |> Fuzz.andMap fuzzer.focus
            |> Fuzz.andMap fuzzer.focus
        )
        "setter_set_set"
        (\{ structure, focus0, focus1 } ->
            (structure
                |> Map.over settable (\_ -> focus0)
                |> Map.over settable (\_ -> focus1)
            )
                |> Expect.equal
                    (structure
                        |> Map.over settable (\_ -> focus1)
                    )
        )



--- test data


type alias Person =
    { name : String
    , age : Int
    , email : Maybe String
    , stuff : List String
    , info : Dict String String
    , things : Array String
    }


personFuzzer : Fuzzer Person
personFuzzer =
    Fuzz.constant
        (\name age email stuff info things ->
            { name = name
            , age = age
            , email = email
            , stuff = stuff
            , info = info
            , things = things
            }
        )
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap (Fuzz.maybe Fuzz.string)
        |> Fuzz.andMap (Fuzz.list Fuzz.string)
        |> Fuzz.andMap
            (dictFuzz { key = Fuzz.string, value = Fuzz.string })
        |> Fuzz.andMap (Fuzz.array Fuzz.string)


stringAlterFuzzer : Fuzzer (String -> String)
stringAlterFuzzer =
    Fuzz.oneOf
        [ Fuzz.map String.append Fuzz.string
        , Fuzz.map (\s -> String.append s << String.reverse) Fuzz.string
        , Fuzz.map (\prefix -> String.append prefix << String.toUpper) Fuzz.string
        , Fuzz.map (\prefix -> String.append prefix << String.toLower) Fuzz.string
        ]


intAlter : Fuzzer (Int -> Int)
intAlter =
    Fuzz.oneOf
        [ Fuzz.map (+) Fuzz.int
        , Fuzz.map (-) Fuzz.int
        , Fuzz.map (*) Fuzz.int
        , Fuzz.map (//) Fuzz.int
        ]



-- fuzzer help


dictFuzz :
    { key : Fuzzer comparableKey
    , value : Fuzzer value
    }
    -> Fuzzer (Dict comparableKey value)
dictFuzz { key, value } =
    Fuzz.list (Fuzz.pair key value)
        |> Fuzz.map Dict.fromList
