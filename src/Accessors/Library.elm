module Accessors.Library exposing (onEach, try, dictEntry)

{-| This library contains common accessors.

@docs onEach, try, dictEntry

-}

import Accessors exposing (..)
import Dict exposing (Dict)


{-| This accessor combinator lets you access values inside lists.

    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Lens as L

    listRecord : { foo : List { bar : Int } }
    listRecord = { foo = [ { bar = 2 }
                         , { bar = 3 }
                         , { bar = 4 }
                         ]
                 }

    get (L.foo << onEach << L.bar) listRecord
    --> [2, 3, 4]

    over (L.foo << onEach << L.bar) ((+) 1) listRecord
    --> { foo = [{ bar = 3 }, { bar = 4 }, { bar = 5 }] }

-}
onEach : Relation attribute built transformed -> Relation (List attribute) built (List transformed)
onEach =
    makeOneToN_ ":[]" List.map List.map


{-| This accessor combinator lets you access values inside Maybe.

    maybeRecord = { foo = Just {bar = 2}
                  , qux = Nothing
                  }

    get (foo << try << bar) maybeRecord
    -- returns Just 2

    get (qux << try << bar) maybeRecord
    -- returns Nothing

    over (foo << try << bar) ((+) 1) maybeRecord
    -- returns {foo = Just {bar = 3}, qux = Nothing}

    over (qux << try << bar) ((+) 1) maybeRecord
    -- returns {foo = Just {bar = 2}, qux = Nothing}

-}
try : Relation attribute built transformed -> Relation (Maybe attribute) built (Maybe transformed)
try =
    makeOneToN_ "?" Maybe.map Maybe.map


{-| This accessor combinator lets you access Dict members.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Lens as L

    dict : Dict String {bar : Int}
    dict = Dict.fromList [("foo", {bar = 2})]

    get (dictEntry "foo") dict
    --> Just {bar = 2}

    get (dictEntry "baz") dict
    --> Nothing

    get (dictEntry "foo" << try << L.bar) dict
    --> Just 2

    set (dictEntry "foo") Nothing dict
    --> Dict.remove "foo" dict

    set (dictEntry "baz" << try << L.bar) 3 dict
    --> dict

-}
dictEntry : comparable -> Relation (Maybe attribute) reachable wrap -> Relation (Dict comparable attribute) reachable wrap
dictEntry k =
    makeOneToOne_ "{}" (Dict.get k) (Dict.update k)
