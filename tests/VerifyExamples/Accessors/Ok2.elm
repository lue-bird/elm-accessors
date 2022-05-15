module VerifyExamples.Accessors.Ok2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors exposing (..)
import Lens as L
import Accessors exposing (..)



maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
maybeRecord = { foo = Ok { bar = 2 }
              , qux = Err "Not an Int"
              }



spec2 : Test.Test
spec2 =
    Test.test "#ok: \n\n    get (L.qux << ok << L.bar) maybeRecord\n    --> Nothing" <|
        \() ->
            Expect.equal
                (
                get (L.qux << ok << L.bar) maybeRecord
                )
                (
                Nothing
                )