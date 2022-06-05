module Laws exposing (tests)

import Accessor exposing (..)
import Array exposing (Array)
import Array.Accessor as Array
import Dict exposing (Dict)
import Dict.Accessor as Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Linear exposing (DirectionLinear(..))
import List.Accessor as List
import Maybe exposing (Maybe)
import Record
import String
import Test exposing (Test, describe, test)


tests : Test
tests =
    Test.describe
        "traversal laws"
        [ optionalExamples
        , settableExamples
        , lensExamples
        , test
            "description"
            (\() ->
                (Record.info << Record.stuff << List.element ( Up, 7 ) << Record.name)
                    |> name
                    |> Expect.equal ".info.stuffelement ↑7.name"
            )
        ]


lensExamples : Test
lensExamples =
    Test.describe
        "lens"
        [ (Record.info << Dict.valueAtString "stuff")
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


{-| Only use `LensConsume` for accessor arguments that are **consumed** – used and then discarded:
-}
type alias LensConsume structure focus =
    Traversal_ structure focus focus


isLens :
    { structure : Fuzzer structure
    , focusAlter : Fuzzer (Alter focus)
    , focus : Fuzzer focus
    }
    -> LensConsume structure focus
    -> Test
isLens fuzzer =
    \settable ->
        Test.describe
            (name settable)
            [ isSettable fuzzer settable

            -- there's Traversal laws in here somewhere but not sure they're expressible in elm
            , lens_get_set fuzzer settable
            , lens_set_get fuzzer settable
            ]


lens_get_set :
    { m | structure : Fuzzer structure }
    -> LensConsume structure focus
    -> Test
lens_get_set fuzzer =
    \lens_ ->
        Test.fuzz
            (Fuzz.constant (\structure -> { structure = structure })
                |> Fuzz.andMap fuzzer.structure
            )
            "lens_get_set"
            (\{ structure } ->
                set lens_ (get lens_ structure) structure
                    |> Expect.equal structure
            )


lens_set_get :
    { m
        | structure : Fuzzer structure
        , focus : Fuzzer focus
    }
    -> LensConsume structure focus
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
                    |> set lens_ focus
                    |> get lens_
                )
                    |> Expect.equal focus
            )


optionalExamples : Test
optionalExamples =
    Test.describe
        "optional"
        [ (Record.email << onJust)
            |> isOptional
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        , (Record.stuff << List.element ( Up, 0 ))
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
    -> Optional_ structure focus
    -> Test
isOptional fuzzer optionalToTest =
    Test.describe
        ("isOptional " ++ name optionalToTest)
        [ optional_identity fuzzer optionalToTest
        , setFocusViewIsFocus fuzzer optionalToTest
        ]


setFocusViewIsFocus :
    { m | structure : Fuzzer structure, focus : Fuzzer focus }
    -> Optional_ structure focus
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
                    |> set optional focus
                    |> get optional
                    |> Expect.equal
                        (structure |> get optional |> Maybe.map (\_ -> focus))
            )


optional_identity :
    { m | structure : Fuzzer structure }
    -> Optional_ structure focus
    -> Test
optional_identity fuzzer optional =
    Test.fuzz
        (Fuzz.constant (\structure -> { structure = structure })
            |> Fuzz.andMap fuzzer.structure
        )
        "optional_identity"
        (\{ structure } ->
            case get optional structure of
                Just focus ->
                    structure
                        |> set optional focus
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
        , check (Record.stuff << List.element ( Up, 0 ))
        , check (Record.stuff << List.elementEach)
        , check (Record.things << Array.element ( Up, 0 ))
        , check (Record.things << Array.elementEach)
        ]


isSettable :
    { structure : Fuzzer structure
    , focusAlter : Fuzzer (Alter focus)
    , focus : Fuzzer focus
    }
    -> Traversal_ structure focus focusView
    -> Test
isSettable fuzzer =
    \settable ->
        Test.describe
            ("isSettable " ++ name settable)
            [ setter_identity fuzzer settable
            , setter_composition fuzzer settable
            , setter_set_set fuzzer settable
            ]


setter_identity :
    { m | structure : Fuzzer structure }
    -> Traversal_ structure focus focusView
    -> Test
setter_identity fuzzer =
    \settable ->
        Test.fuzz
            (Fuzz.constant (\structure -> { structure = structure })
                |> Fuzz.andMap fuzzer.structure
            )
            "setter_identity"
            (\{ structure } ->
                over settable identity structure
                    |> Expect.equal structure
            )


setter_composition :
    { m
        | structure : Fuzzer structure
        , focusAlter : Fuzzer (Alter focus)
    }
    -> Traversal_ structure focus focusView
    -> Test
setter_composition fuzzer =
    \settable ->
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
                    |> over settable alter0
                    |> over settable alter1
                )
                    |> Expect.equal
                        (over settable (alter0 >> alter1) structure)
            )


setter_set_set :
    { m
        | structure : Fuzzer structure
        , focus : Fuzzer focus
    }
    -> Traversal_ structure focus focusView
    -> Test
setter_set_set fuzzer =
    \settable ->
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
                    |> set settable focus0
                    |> set settable focus1
                )
                    |> Expect.equal
                        (set settable focus1 structure)
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
            [ \maybe ->
                maybe
                    |> Maybe.andThen String.toInt
                    |> Maybe.map String.fromInt
            ]
        )


stringAlterFuzzer : Fuzzer (Alter String)
stringAlterFuzzer =
    Fuzz.oneOf
        [ Fuzz.map String.append
            Fuzz.string
        , Fuzz.map (\s -> String.append s << String.reverse)
            Fuzz.string
        , Fuzz.map (\prefix -> String.append prefix << String.toUpper)
            Fuzz.string
        , Fuzz.map (\prefix -> String.append prefix << String.toLower)
            Fuzz.string

        -- , Fuzz.map String.reverse string
        -- , Fuzz.constant String.toUpper
        -- , Fuzz.constant String.toLower
        ]


intAlter : Fuzzer (Alter Int)
intAlter =
    Fuzz.oneOf
        [ Fuzz.map (+)
            Fuzz.int
        , Fuzz.map (-)
            Fuzz.int
        , Fuzz.map (*)
            Fuzz.int
        , Fuzz.map (//)
            Fuzz.int
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
