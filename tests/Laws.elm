module Laws exposing (tests)

import Array exposing (Array)
import Array.Map
import Dict exposing (Dict)
import Dict.Map
import Expect
import Fuzz exposing (Fuzzer)
import List.Map
import Map exposing (onJust)
import Maybe exposing (Maybe)
import Record
import String
import Test exposing (Test, test)


tests : Test
tests =
    Test.describe
        "traversal laws"
        [ prismExamples
        , settableExamples
        , lensExamples
        , test "description"
            (\() ->
                (Record.info << Record.stuff << List.Map.element 7 << Record.name)
                    |> Map.description
                    |> String.join ")"
                    |> Expect.equal "info)stuff)element 7)name"
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
    , focusAlter : Fuzzer (Alter focus)
    , focus : Fuzzer focus
    }
    -> Map.PartMappingToSameType structure focus focus
    -> Test
isLens fuzzer =
    \settable ->
        Test.describe
            (settable |> Map.description |> String.join ")")
            [ isSettable fuzzer settable

            -- there's Traversal laws in here somewhere but not sure they're expressible in elm
            , lens_get_set fuzzer settable
            , lens_set_get fuzzer settable
            ]


lens_get_set :
    { m | structure : Fuzzer structure }
    -> Map.PartMappingToSameType structure focus focus
    -> Test
lens_get_set fuzzer =
    \lens_ ->
        Test.fuzz
            (Fuzz.constant (\structure -> { structure = structure })
                |> Fuzz.andMap fuzzer.structure
            )
            "lens_get_set"
            (\{ structure } ->
                structure
                    |> Map.over lens_ (\_ -> structure |> Map.view lens_)
                    |> Expect.equal structure
            )


lens_set_get :
    { m
        | structure : Fuzzer structure
        , focus : Fuzzer focus
    }
    -> Map.PartMappingToSameType structure focus focus
    -> Test
lens_set_get fuzzer =
    \lens_ ->
        Test.fuzz
            (Fuzz.constant
                (\structure focus ->
                    { structure = structure
                    , focus = focus
                    }
                )
                |> Fuzz.andMap fuzzer.structure
                |> Fuzz.andMap fuzzer.focus
            )
            "lens_set_get"
            (\{ structure, focus } ->
                (structure
                    |> Map.over lens_ (\_ -> focus)
                    |> Map.view lens_
                )
                    |> Expect.equal focus
            )


prismExamples : Test
prismExamples =
    Test.describe
        "prism"
        [ (Record.info << Dict.Map.valueAtString "stuff")
            |> isPrism
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        , (Record.email << onJust)
            |> isPrism
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        , (Record.stuff << List.Map.element 0)
            |> isPrism
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        ]


isPrism :
    { structure : Fuzzer structure
    , focusAlter : Fuzzer (Alter focus)
    , focus : Fuzzer focus
    }
    -> Map.Alter structure focus focus
    -> Test
isPrism fuzzer optionalToTest =
    Test.describe
        ("isOptional "
            ++ (optionalToTest |> Map.description |> String.join ")")
        )
        [ optional_identity fuzzer optionalToTest
        , setFocusViewIsFocus fuzzer optionalToTest
        ]


setFocusViewIsFocus :
    { m | structure : Fuzzer structure, focus : Fuzzer focus }
    -> Map.Alter structure focus focus
    -> Test
setFocusViewIsFocus fuzzer =
    \optional ->
        Test.fuzz
            (Fuzz.constant (\structure focus -> { structure = structure, focus = focus })
                |> Fuzz.andMap fuzzer.structure
                |> Fuzz.andMap fuzzer.focus
            )
            "setFocusViewIsFocus"
            (\{ structure, focus } ->
                structure
                    |> Map.over optional (\_ -> focus)
                    |> Map.view optional
                    |> Expect.equal
                        (structure |> Map.view optional |> Maybe.Map (\_ -> focus))
            )


optional_identity :
    { m | structure : Fuzzer structure }
    -> Map.Alter structure focus focus
    -> Test
optional_identity fuzzer optional =
    Test.fuzz
        (Fuzz.constant (\structure -> { structure = structure })
            |> Fuzz.andMap fuzzer.structure
        )
        "optional_identity"
        (\{ structure } ->
            case structure |> Map.view optional of
                Just focus ->
                    structure
                        |> Map.over optional (\_ -> focus)
                        |> Expect.equal structure

                Nothing ->
                    Expect.pass
        )


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
    , focusAlter : Fuzzer (Alter focus)
    , focus : Fuzzer focus
    }
    -> Map.ElementsMappingToSameType structure focus view focus
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
    -> Map.ElementsMappingToSameType structure focus view focus
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
        , focusAlter : Fuzzer (Alter focus)
    }
    -> Map.ElementsMappingToSameType structure focus view focus
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
    -> Map.ElementsMappingToSameType structure focus view focus
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


type alias Alter a =
    a -> a


maybeStringAlterFuzzer : Fuzzer (Alter (Maybe String))
maybeStringAlterFuzzer =
    Fuzz.oneOf
        (List.Map Fuzz.constant
            [ Maybe.andThen String.toInt >> Maybe.Map String.fromInt
            ]
        )


stringAlterFuzzer : Fuzzer (Alter String)
stringAlterFuzzer =
    Fuzz.oneOf
        [ Fuzz.Map String.append Fuzz.string
        , Fuzz.Map (\s -> String.append s << String.reverse) Fuzz.string
        , Fuzz.Map (\prefix -> String.append prefix << String.toUpper) Fuzz.string
        , Fuzz.Map (\prefix -> String.append prefix << String.toLower) Fuzz.string
        ]


intAlter : Fuzzer (Alter Int)
intAlter =
    Fuzz.oneOf
        [ Fuzz.Map (+) Fuzz.int
        , Fuzz.Map (-) Fuzz.int
        , Fuzz.Map (*) Fuzz.int
        , Fuzz.Map (//) Fuzz.int
        ]



-- fuzzer help


dictFuzz :
    { key : Fuzzer comparableKey
    , value : Fuzzer value
    }
    -> Fuzzer (Dict comparableKey value)
dictFuzz { key, value } =
    Fuzz.list (Fuzz.tuple ( key, value ))
        |> Fuzz.Map Dict.fromList
