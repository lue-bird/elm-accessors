module Laws exposing (suite)

import Accessor exposing (Lens, TraversalConsume, mapOver)
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
          -- , isPrism (Record.email << Accessor.onJust)
          settableTest
        , lens
        , test
            "description"
            (\() ->
                (Record.info << Record.stuff << List.element ( Up, 7 ) << Record.name)
                    |> Accessor.description
                    |> Accessor.descriptionToString
                    |> Expect.equal "record>.info:record>.stuff:List>element ↑7:record>.name"
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
            (Record.email << Accessor.onJust)
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


isSettable :
    TraversalConsume structure focus focusView
    -> Fuzzer structure
    -> Fuzzer (Alter focus)
    -> Fuzzer focus
    -> Test
isSettable settable structureFuzzer alterFuzzer focusFuzzer =
    Test.describe
        ("isSettable: " ++ (settable |> Accessor.description |> Accessor.descriptionToString))
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


{-| Only use `LensConsume` for accessor arguments that are **consumed** – used and then discarded:
-}
type alias LensConsume structure focus =
    TraversalConsume structure focus focus


isLens :
    LensConsume structure focus
    -> Fuzzer structure
    -> Fuzzer (Alter focus)
    -> Fuzzer focus
    -> Test
isLens settable structureFuzzer alterFuzzer focusFuzzer =
    Test.describe
        ("isLens: " ++ (settable |> Accessor.description |> Accessor.descriptionToString))
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
    TraversalConsume structure focus focusView
    -> structure
    -> Bool
setter_identity settable structure =
    (structure |> Accessor.mapOver settable identity) == structure


setter_composition :
    TraversalConsume structure focus focusView
    -> structure
    -> Alter focus
    -> Alter focus
    -> Bool
setter_composition settable structure alter0 alter1 =
    (structure
        |> Accessor.mapOver settable alter1
        |> Accessor.mapOver settable alter0
    )
        == (structure |> Accessor.mapOver settable (alter0 << alter1))


setter_set_set :
    TraversalConsume structure focus focusView
    -> structure
    -> focus
    -> focus
    -> Bool
setter_set_set settable structure a b =
    (structure
        |> Accessor.mapOver settable (\_ -> a)
        |> Accessor.mapOver settable (\_ -> b)
    )
        == (structure |> Accessor.mapOver settable (\_ -> b))


lens_set_get : LensConsume structure focus -> structure -> Bool
lens_set_get lens_ structure =
    (structure
        |> Accessor.mapOver lens_
            (\_ -> structure |> Accessor.view lens_)
    )
        == structure


lens_get_set : LensConsume structure focus -> structure -> focus -> Bool
lens_get_set lens_ structure focus =
    (structure
        |> Accessor.mapOver lens_ (\_ -> focus)
        |> Accessor.view lens_
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
