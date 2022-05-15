module VerifyExamples.Accessors.Err2 exposing (..)

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
    Test.test "#err: \n\n    get (L.qux << err) maybeRecord\n    --> Just \"Not an Int\"" <|
        \() ->
            Expect.equal
                (
                get (L.qux << err) maybeRecord
                )
                (
                Just "Not an Int"
                )