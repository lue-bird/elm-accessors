module Laws exposing (tests)

import Array exposing (Array)
import Array.Reach
import Dict exposing (Dict)
import Dict.Reach
import Expect
import Fuzz exposing (Fuzzer)
import Linear exposing (DirectionLinear(..))
import List.Reach
import Maybe exposing (Maybe)
import Reach exposing (onJust)
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
                (Record.info << Record.stuff << List.Reach.element ( Up, 7 ) << Record.name)
                    |> Reach.description
                    |> String.join ")"
                    |> Expect.equal "info)stuff)element ↑7)name"
            )
        ]


lensExamples : Test
lensExamples =
    Test.describe
        "lens"
        [ (Record.info << Dict.Reach.valueAtString "stuff")
            |> isLens
                { structure = personFuzzer
                , focusAlter = maybeStringAlterFuzzer
                , focus = Fuzz.maybe Fuzz.string
                }
        , Record.name
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
    -> Reach.PartMappingToSameType structure focus focus
    -> Test
isLens fuzzer =
    \settable ->
        Test.describe
            (settable |> Reach.description |> String.join ")")
            [ isSettable fuzzer settable

            -- there's Traversal laws in here somewhere but not sure they're expressible in elm
            , lens_get_set fuzzer settable
            , lens_set_get fuzzer settable
            ]


lens_get_set :
    { m | structure : Fuzzer structure }
    -> Reach.PartMappingToSameType structure focus focus
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
                    |> Reach.mapOver lens_ (\_ -> structure |> Reach.view lens_)
                    |> Expect.equal structure
            )


lens_set_get :
    { m
        | structure : Fuzzer structure
        , focus : Fuzzer focus
    }
    -> Reach.PartMappingToSameType structure focus focus
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
                    |> Reach.mapOver lens_ (\_ -> focus)
                    |> Reach.view lens_
                )
                    |> Expect.equal focus
            )


prismExamples : Test
prismExamples =
    Test.describe
        "optional"
        [ (Record.email << onJust)
            |> isOptional
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        , (Record.stuff << List.Reach.element ( Up, 0 ))
            |> isOptional
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        ]


isOptional :
    { structure : Fuzzer structure
    , focusAlter : Fuzzer (Alter focus)
    , focus : Fuzzer focus
    }
    -> Reach.MaybeMappingToSameType structure focus focus
    -> Test
isOptional fuzzer optionalToTest =
    Test.describe
        ("isOptional "
            ++ (optionalToTest |> Reach.description |> String.join ")")
        )
        [ optional_identity fuzzer optionalToTest
        , setFocusViewIsFocus fuzzer optionalToTest
        ]


setFocusViewIsFocus :
    { m | structure : Fuzzer structure, focus : Fuzzer focus }
    -> Reach.MaybeMappingToSameType structure focus focus
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
                    |> Reach.mapOver optional (\_ -> focus)
                    |> Reach.view optional
                    |> Expect.equal
                        (structure |> Reach.view optional |> Maybe.map (\_ -> focus))
            )


optional_identity :
    { m | structure : Fuzzer structure }
    -> Reach.MaybeMappingToSameType structure focus focus
    -> Test
optional_identity fuzzer optional =
    Test.fuzz
        (Fuzz.constant (\structure -> { structure = structure })
            |> Fuzz.andMap fuzzer.structure
        )
        "optional_identity"
        (\{ structure } ->
            case structure |> Reach.view optional of
                Just focus ->
                    structure
                        |> Reach.mapOver optional (\_ -> focus)
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
        , check (Record.stuff << List.Reach.element ( Up, 0 ))
        , check (Record.stuff << List.Reach.elementEach)
        , check (Record.things << Array.Reach.element ( Up, 0 ))
        , check (Record.things << Array.Reach.elementEach)
        ]


isSettable :
    { structure : Fuzzer structure
    , focusAlter : Fuzzer (Alter focus)
    , focus : Fuzzer focus
    }
    -> Reach.ElementsMappingToSameType structure focus view focus
    -> Test
isSettable fuzzer settable =
    Test.describe
        ("isSettable "
            ++ (settable |> Reach.description |> String.join ")")
        )
        [ setter_identity fuzzer settable
        , setter_composition fuzzer settable
        , setter_set_set fuzzer settable
        ]


setter_identity :
    { m | structure : Fuzzer structure }
    -> Reach.ElementsMappingToSameType structure focus view focus
    -> Test
setter_identity fuzzer settable =
    Test.fuzz
        (Fuzz.constant (\structure -> { structure = structure })
            |> Fuzz.andMap fuzzer.structure
        )
        "setter_identity"
        (\{ structure } ->
            structure
                |> Reach.mapOver settable identity
                |> Expect.equal structure
        )


setter_composition :
    { m
        | structure : Fuzzer structure
        , focusAlter : Fuzzer (Alter focus)
    }
    -> Reach.ElementsMappingToSameType structure focus view focus
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
                |> Reach.mapOver settable alter0
                |> Reach.mapOver settable alter1
            )
                |> Expect.equal
                    (structure
                        |> Reach.mapOver settable (alter0 >> alter1)
                    )
        )


setter_set_set :
    { m
        | structure : Fuzzer structure
        , focus : Fuzzer focus
    }
    -> Reach.ElementsMappingToSameType structure focus view focus
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
                |> Reach.mapOver settable (\_ -> focus0)
                |> Reach.mapOver settable (\_ -> focus1)
            )
                |> Expect.equal
                    (structure
                        |> Reach.mapOver settable (\_ -> focus1)
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
        (List.map Fuzz.constant
            [ Maybe.andThen String.toInt >> Maybe.map String.fromInt
            ]
        )


stringAlterFuzzer : Fuzzer (Alter String)
stringAlterFuzzer =
    Fuzz.oneOf
        [ Fuzz.map String.append Fuzz.string
        , Fuzz.map (\s -> String.append s << String.reverse) Fuzz.string
        , Fuzz.map (\prefix -> String.append prefix << String.toUpper) Fuzz.string
        , Fuzz.map (\prefix -> String.append prefix << String.toLower) Fuzz.string
        ]


intAlter : Fuzzer (Alter Int)
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
    Fuzz.list (Fuzz.tuple ( key, value ))
        |> Fuzz.map Dict.fromList
