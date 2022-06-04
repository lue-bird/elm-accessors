module Laws exposing (suite)

import Accessor exposing (Lens, TraversalConsume, mapOver, onJust)
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
            (Record.email << onJust)
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
    TraversalConsume structure focus structure focus focusView focusView focusViewNamed
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
                    |> mapIdentityIsIdentity settable
                    |> Expect.equal True
            )
        , Test.fuzz
            (Fuzz.tuple3 ( structureFuzzer, alterFuzzer, alterFuzzer ))
            "composition"
            (\( structure, alter0, alter1 ) ->
                mapMultipleIsMapComposition settable structure alter0 alter1
                    |> Expect.equal True
            )
        , Test.fuzz
            (Fuzz.tuple3 ( structureFuzzer, focusFuzzer, focusFuzzer ))
            "set_set"
            (\( structure, focus0, focus1 ) ->
                setMultipleIsSetLast settable structure focus0 focus1
                    |> Expect.equal True
            )
        ]


{-| Only use `LensConsume` for accessor arguments that are **consumed** – used and then discarded:
-}
type alias LensConsume structure focus structureMapped focusMapped focusNamed =
    TraversalConsume structure focus structureMapped focusMapped focus focus focusNamed


isLens :
    LensConsume structure focus structure focus focusNamed
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
                    |> lensSetAsItsFocus settable
                    |> Expect.equal True
            )
        , Test.fuzz
            (Fuzz.tuple ( structureFuzzer, focusFuzzer ))
            "lens_get_set"
            (\( structure, focus ) ->
                lensSetFocusViewIsFocus settable structure focus
                    |> Expect.equal True
            )
        ]


mapIdentityIsIdentity :
    TraversalConsume structure focus structure focus focusView focusView focusViewNamed
    -> structure
    -> Bool
mapIdentityIsIdentity settable structure =
    (structure
        |> Accessor.mapOver settable identity
    )
        == structure


mapMultipleIsMapComposition :
    TraversalConsume structure focus structure focus focusView focusView focusViewNamed
    -> structure
    -> Alter focus
    -> Alter focus
    -> Bool
mapMultipleIsMapComposition settable structure alter0 alter1 =
    (structure
        |> Accessor.mapOver settable alter0
        |> Accessor.mapOver settable alter1
    )
        == (structure
                |> Accessor.mapOver settable (alter0 >> alter1)
           )


setMultipleIsSetLast :
    TraversalConsume structure focus structure focus focusView focusView focusViewNamed
    -> structure
    -> focus
    -> focus
    -> Bool
setMultipleIsSetLast settable structure focus0 focus1 =
    (structure
        |> Accessor.mapOver settable (\_ -> focus0)
        |> Accessor.mapOver settable (\_ -> focus1)
    )
        == (structure
                |> Accessor.mapOver settable (\_ -> focus1)
           )


lensSetAsItsFocus :
    LensConsume structure focus structure focus focusNamed
    -> structure
    -> Bool
lensSetAsItsFocus lens_ structure =
    (structure
        |> Accessor.mapOver lens_
            (\_ ->
                structure |> Accessor.view lens_
            )
    )
        == structure


lensSetFocusViewIsFocus :
    LensConsume structure focus structure focus focusNamed
    -> structure
    -> focus
    -> Bool
lensSetFocusViewIsFocus lens_ structure focus =
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
