module Laws exposing (tests)

import Accessor exposing (Lens, TraversalConsume, mapOver, onJust, view)
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
        [ prismExamples
        , settableExamples
        , lensExamples
        , test
            "description"
            (\() ->
                (Record.info << Record.stuff << List.element ( Up, 7 ) << Record.name)
                    |> Accessor.description
                    |> Accessor.descriptionToString
                    |> Expect.equal ".info>.stuff>element ↑7>.name"
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
    TraversalConsume structure focus focus


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
            (settable |> Accessor.description |> Accessor.descriptionToString)
            [ settable |> isSettable fuzzer

            -- there's Traversal laws in here somewhere but not sure they're expressible in elm
            , settable |> lensSetAsItsFocus { structure = fuzzer.structure }
            , settable
                |> lensSetFocusViewIsFocus
                    { structure = fuzzer.structure
                    , focus = fuzzer.focus
                    }
            ]


lensSetAsItsFocus :
    { structure : Fuzzer structure }
    -> LensConsume structure focus
    -> Test
lensSetAsItsFocus fuzzer =
    \lens_ ->
        Test.fuzz
            (Fuzz.constant (\structure -> { structure = structure })
                |> Fuzz.andMap fuzzer.structure
            )
            "lensSetAsItsFocus"
            (\{ structure } ->
                (structure
                    |> Accessor.mapOver lens_
                        (\_ ->
                            structure |> Accessor.view lens_
                        )
                )
                    |> Expect.equal structure
            )


lensSetFocusViewIsFocus :
    { structure : Fuzzer structure
    , focus : Fuzzer focus
    }
    -> LensConsume structure focus
    -> Test
lensSetFocusViewIsFocus fuzzer =
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
            "lensSetFocusViewIsFocus"
            (\{ structure, focus } ->
                (structure
                    |> Accessor.mapOver lens_ (\_ -> focus)
                    |> Accessor.view lens_
                )
                    |> Expect.equal focus
            )


{-| Only use `LensConsume` for accessor arguments that are **consumed** – used and then discarded:
-}
type alias OptionalConsume structure focus =
    TraversalConsume structure focus (Maybe focus)


prismExamples : Test
prismExamples =
    Test.describe
        "prism"
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
    -> OptionalConsume structure focus
    -> Test
isOptional fuzzer =
    \optionalToTest ->
        Test.describe
            ("isOptional " ++ (optionalToTest |> Accessor.description |> Accessor.descriptionToString))
            [ optionalToTest
                |> setJustViewIsIdentity
                    { structure = fuzzer.structure }
            , optionalToTest
                |> setFocusViewIsFocus
                    { structure = fuzzer.structure
                    , focus = fuzzer.focus
                    }
            ]


setFocusViewIsFocus :
    { structure : Fuzzer structure, focus : Fuzzer focus }
    -> OptionalConsume structure focus
    -> Test
setFocusViewIsFocus fuzzer =
    \prism ->
        Test.fuzz
            (Fuzz.constant (\structure focus -> { structure = structure, focus = focus })
                |> Fuzz.andMap fuzzer.structure
                |> Fuzz.andMap fuzzer.focus
            )
            "setFocusViewIsFocus"
            (\{ structure, focus } ->
                structure
                    |> mapOver prism (\_ -> focus)
                    |> view prism
                    |> Expect.equal
                        (structure |> view prism |> Maybe.map (\_ -> focus))
            )


setJustViewIsIdentity :
    { structure : Fuzzer structure }
    -> OptionalConsume structure focus
    -> Test
setJustViewIsIdentity fuzzer =
    \prism ->
        Test.fuzz
            (Fuzz.constant (\structure -> { structure = structure })
                |> Fuzz.andMap fuzzer.structure
            )
            "setJustViewIsIdentity"
            (\{ structure } ->
                case structure |> view prism of
                    Nothing ->
                        Expect.pass

                    Just focusView ->
                        structure
                            |> mapOver prism (\_ -> focusView)
                            |> Expect.equal structure
            )


settableExamples : Test
settableExamples =
    Test.describe
        "settable"
        [ (Record.email << onJust)
            |> isSettable
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        , (Record.stuff << List.element ( Up, 0 ))
            |> isSettable
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        , (Record.stuff << List.elementEach)
            |> isSettable
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        , (Record.things << Array.element ( Up, 0 ))
            |> isSettable
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        , (Record.things << Array.elementEach)
            |> isSettable
                { structure = personFuzzer
                , focusAlter = stringAlterFuzzer
                , focus = Fuzz.string
                }
        ]


isSettable :
    { structure : Fuzzer structure
    , focusAlter : Fuzzer (Alter focus)
    , focus : Fuzzer focus
    }
    -> TraversalConsume structure focus focusView
    -> Test
isSettable fuzzer =
    \settable ->
        Test.describe
            ("isSettable " ++ (settable |> Accessor.description |> Accessor.descriptionToString))
            [ settable
                |> mapIdentityIsIdentity { structure = fuzzer.structure }
            , settable
                |> mapMultipleIsMapComposition
                    { structure = fuzzer.structure
                    , focusAlter = fuzzer.focusAlter
                    }
            , settable
                |> setMultipleIsSetLast
                    { structure = fuzzer.structure
                    , focus = fuzzer.focus
                    }
            ]


mapIdentityIsIdentity :
    { structure : Fuzzer structure }
    -> TraversalConsume structure focus focusView
    -> Test
mapIdentityIsIdentity fuzzer =
    \settable ->
        Test.fuzz
            (Fuzz.constant (\structure -> { structure = structure })
                |> Fuzz.andMap fuzzer.structure
            )
            "mapIdentityIsIdentity"
            (\{ structure } ->
                (structure
                    |> Accessor.mapOver settable identity
                )
                    |> Expect.equal structure
            )


mapMultipleIsMapComposition :
    { structure : Fuzzer structure
    , focusAlter : Fuzzer (Alter focus)
    }
    -> TraversalConsume structure focus focusView
    -> Test
mapMultipleIsMapComposition fuzzer =
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
            "mapMultipleIsMapComposition"
            (\{ structure, alter0, alter1 } ->
                (structure
                    |> Accessor.mapOver settable alter0
                    |> Accessor.mapOver settable alter1
                )
                    |> Expect.equal
                        (structure
                            |> Accessor.mapOver settable (alter0 >> alter1)
                        )
            )


setMultipleIsSetLast :
    { structure : Fuzzer structure
    , focus : Fuzzer focus
    }
    -> TraversalConsume structure focus focusView
    -> Test
setMultipleIsSetLast fuzzer =
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
            "setMultipleIsSetLast"
            (\{ structure, focus0, focus1 } ->
                (structure
                    |> Accessor.mapOver settable (\_ -> focus0)
                    |> Accessor.mapOver settable (\_ -> focus1)
                )
                    |> Expect.equal
                        (structure
                            |> Accessor.mapOver settable (\_ -> focus1)
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
