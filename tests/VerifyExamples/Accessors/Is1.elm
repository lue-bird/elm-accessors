module VerifyExamples.Accessors.Is1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors exposing (..)







spec1 : Test.Test
spec1 =
    Test.test "#is: \n\n    [\"Stuff\", \"things\"]\n        |> is (at 2)\n    --> False" <|
        \() ->
            Expect.equal
                (
                ["Stuff", "things"]
                    |> is (at 2)
                )
                (
                False
                )