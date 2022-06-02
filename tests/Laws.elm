module Laws exposing (suite)

import Accessor as A exposing (Lens)
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
import Test exposing (Test, test)


suite : Test
suite =
    Test.describe
        "lens laws"
        [ -- TODO: How to express laws for Prisms
          -- , isPrism (Record.email << A.onJust)
          settableTest
        , lens
        , test
            "description"
            (\() ->
                (Record.info << Record.stuff << List.element ( Up, 7 ) << Record.name)
                    |> A.description
                    |> A.descriptionToString
                    |> Expect.equal "record>.info:record>.stuff:List>element ↑7:Maybe>Just:record>.name"
            )
        ]


lens : Test
lens =
    Test.describe
        "lens"
        [ isLens
            (Record.info << Dict.valueAtString "stuff")
            personFuzzer
            maybeStringAlter
            (Fuzz.maybe Fuzz.string)
        , isLens Record.name personFuzzer stringAlter Fuzz.string
        , isLens Record.age personFuzzer intAlter Fuzz.int
        ]


settableTest : Test
settableTest =
    Test.describe
        "setable"
        [ isSettable
            (Record.email << A.onJust)
            personFuzzer
            stringAlter
            Fuzz.string
        , isSettable
            (Record.stuff << List.element ( Up, 0 ))
            personFuzzer
            stringAlter
            Fuzz.string
        , isSettable
            (Record.stuff << List.elementEach)
            personFuzzer
            stringAlter
            Fuzz.string
        , isSettable
            (Record.things << Array.element ( Up, 0 ))
            personFuzzer
            stringAlter
            Fuzz.string
        , isSettable
            (Record.things << Array.elementEach)
            personFuzzer
            stringAlter
            Fuzz.string
        ]


type alias Alter a =
    a -> a


stringAlter : Fuzzer (Alter String)
stringAlter =
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


maybeStringAlter : Fuzzer (Alter (Maybe String))
maybeStringAlter =
    Fuzz.oneOf
        (List.map Fuzz.constant
            [ \maybe ->
                maybe
                    |> Maybe.andThen String.toInt
                    |> Maybe.map String.fromInt
            ]
        )


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


type alias Settable structure transformedStructure focus =
    A.Relation focus focus focus
    -> A.Relation structure focus transformedStructure


isSettable :
    Settable structure transformed attribute
    -> Fuzzer structure
    -> Fuzzer (Alter attribute)
    -> Fuzzer attribute
    -> Test
isSettable settable structureFuzzer alterFuzzer focusFuzzer =
    Test.describe
        ("isSettable: " ++ (settable |> A.description |> A.descriptionToString))
        [ Test.fuzz
            structureFuzzer
            "identity"
            (\structure ->
                structure
                    |> setter_identity settable
                    |> Expect.equal True
            )
        , Test.fuzz
            (Fuzz.tuple3 ( structureFuzzer, alterFuzzer, alterFuzzer ))
            "composition"
            (\( structure, alter0, alter1 ) ->
                setter_composition settable structure alter0 alter1
                    |> Expect.equal True
            )
        , Test.fuzz
            (Fuzz.tuple3 ( structureFuzzer, focusFuzzer, focusFuzzer ))
            "set_set"
            (\( structure, focus0, focus1 ) ->
                setter_set_set settable structure focus0 focus1
                    |> Expect.equal True
            )
        ]


{-| Simplified version of [`Lens`](#Lens)
which breaks type inference when used in complex compositions.
-}
type alias LensFinal structure focus =
    Lens structure focus focus focus


isLens :
    LensFinal structure attribute
    -> Fuzzer structure
    -> Fuzzer (Alter attribute)
    -> Fuzzer attribute
    -> Test
isLens settable structureFuzzer alterFuzzer focusFuzzer =
    Test.describe
        ("isLens: " ++ (settable |> A.description |> A.descriptionToString))
        [ isSettable settable structureFuzzer alterFuzzer focusFuzzer

        -- there's Traversal laws in here somewhere but not sure they're expressible in elm
        , Test.fuzz
            structureFuzzer
            "lens_set_get"
            (\fuzzed ->
                fuzzed
                    |> lens_set_get settable
                    |> Expect.equal True
            )
        , Test.fuzz
            (Fuzz.tuple ( structureFuzzer, focusFuzzer ))
            "lens_get_set"
            (\( structure, focus ) ->
                lens_get_set settable structure focus
                    |> Expect.equal True
            )
        ]


setter_identity :
    Settable structure transformed attribute
    -> structure
    -> Bool
setter_identity settable structure =
    (structure |> A.mapOver settable identity) == structure


setter_composition :
    Settable structure transformed attribute
    -> structure
    -> Alter attribute
    -> Alter attribute
    -> Bool
setter_composition settable structure alter0 alter1 =
    (structure
        |> A.mapOver settable alter1
        |> A.mapOver settable alter0
    )
        == (structure |> A.mapOver settable (alter0 << alter1))


setter_set_set :
    Settable structure transformed focus
    -> structure
    -> focus
    -> focus
    -> Bool
setter_set_set settable structure a b =
    (structure
        |> A.mapOver settable (\_ -> a)
        |> A.mapOver settable (\_ -> b)
    )
        == (structure |> A.mapOver settable (\_ -> b))


lens_set_get : LensFinal structure attribute -> structure -> Bool
lens_set_get lens_ structure =
    (structure
        |> A.mapOver lens_
            (\_ -> structure |> A.view lens_)
    )
        == structure


lens_get_set : LensFinal structure focus -> structure -> focus -> Bool
lens_get_set lens_ structure focus =
    (structure
        |> A.mapOver lens_ (\_ -> focus)
        |> A.view lens_
    )
        == focus



---


dictFuzz :
    { key : Fuzzer comparableKey
    , value : Fuzzer value
    }
    -> Fuzzer (Dict comparableKey value)
dictFuzz { key, value } =
    Fuzz.list (Fuzz.tuple ( key, value ))
        |> Fuzz.map Dict.fromList
