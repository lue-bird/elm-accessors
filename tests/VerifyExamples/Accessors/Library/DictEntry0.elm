module VerifyExamples.Accessors.Library.DictEntry0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors.Library exposing (..)
import Lens as L
import Accessors.Library exposing (..)
import Accessors exposing (..)
import Dict exposing (Dict)



dict : Dict String {bar : Int}
dict = Dict.fromList [("foo", {bar = 2})]



spec0 : Test.Test
spec0 =
    Test.test "#dictEntry: \n\n    set (dictEntry \"baz\" << try << L.bar) 3 dict\n    --> dict" <|
        \() ->
            Expect.equal
                (
                set (dictEntry "baz" << try << L.bar) 3 dict
                )
                (
                dict
                )