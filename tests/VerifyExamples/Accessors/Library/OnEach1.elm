module VerifyExamples.Accessors.Library.OnEach1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors.Library exposing (..)
import Lens as L
import Accessors.Library exposing (..)
import Accessors exposing (..)



listRecord : { foo : List { bar : Int } }
listRecord = { foo = [ { bar = 2 }
                     , { bar = 3 }
                     , { bar = 4 }
                     ]
             }



spec1 : Test.Test
spec1 =
    Test.test "#onEach: \n\n    get (L.foo << onEach << L.bar) listRecord\n    --> [2, 3, 4]" <|
        \() ->
            Expect.equal
                (
                get (L.foo << onEach << L.bar) listRecord
                )
                (
                [2, 3, 4]
                )