module Accessors.Library exposing
    ( try, def, or
    , values, keyed, key, dictEntry
    , onEach, onEachIx, at
    , onEvery, onEveryIx, ix
    , fst, snd
    )

{-| This library contains common accessors.


## Maybe

@docs try, def, or


## Dict

@docs values, keyed, key, dictEntry


## List

@docs onEach, onEachIx, at


## Array

@docs onEvery, onEveryIx, ix


## Tuples

@docs fst, snd

-}

import Accessors exposing (..)
import Array exposing (Array)
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


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Lens as L

    listRecord : {foo : List {bar : Int}}
    listRecord = { foo = [ {bar = 2}
                         , {bar = 3}
                         , {bar = 4}
                         ]
                 }

    multiplyIfGTOne : (Int, { bar : Int }) -> (Int, { bar : Int })
    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
        if idx > 0 then
            ( idx, { bar = bar * 10 } )
        else
            (idx, rec)


    get (L.foo << onEachIx) listRecord
    --> [(0, {bar = 2}), (1, {bar = 3}), (2, {bar = 4})]

    over (L.foo << onEachIx) multiplyIfGTOne listRecord
    --> {foo = [{bar = 2}, {bar = 30}, {bar = 40}]}

    get (L.foo << onEachIx << snd << L.bar) listRecord
    --> [2, 3, 4]

    over (L.foo << onEachIx << snd << L.bar) ((+) 1) listRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
onEachIx : Relation ( Int, attribute ) reachable built -> Relation (List attribute) reachable (List built)
onEachIx =
    makeOneToN_ "#[]"
        (\fn ->
            List.indexedMap
                (\idx -> Tuple.pair idx >> fn)
        )
        (\fn ->
            List.indexedMap
                (\idx -> Tuple.pair idx >> fn >> Tuple.second)
        )


{-| This accessor combinator lets you access values inside Array.

    import Array exposing (Array)
    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Lens as L

    arrayRecord : {foo : Array {bar : Int}}
    arrayRecord =
        { foo =
            Array.fromList [{ bar = 2 }, { bar = 3 }, {bar = 4}]
        }

    get (L.foo << onEvery << L.bar) arrayRecord
    --> Array.fromList [2, 3, 4]

    over (L.foo << onEvery << L.bar) ((+) 1) arrayRecord
    --> {foo = Array.fromList [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
onEvery : Relation attribute built transformed -> Relation (Array attribute) built (Array transformed)
onEvery =
    makeOneToN_ "[]" Array.map Array.map


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Lens as L
    import Array exposing (Array)

    arrayRecord : { foo : Array { bar : Int } }
    arrayRecord = { foo = [ {bar = 2}
                          , {bar = 3}
                          , {bar = 4}
                          ] |> Array.fromList
                  }

    multiplyIfGTOne : (Int, { bar : Int }) -> (Int, { bar : Int })
    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
        if idx > 0 then
            ( idx, { bar = bar * 10 } )
        else
            (idx, rec)


    get (L.foo << onEveryIx) arrayRecord
    --> [(0, {bar = 2}), (1, {bar = 3}), (2, {bar = 4})] |> Array.fromList

    over (L.foo << onEveryIx) multiplyIfGTOne arrayRecord
    --> {foo = [{bar = 2}, {bar = 30}, {bar = 40}] |> Array.fromList}

    get (L.foo << onEveryIx << snd << L.bar) arrayRecord
    --> [2, 3, 4] |> Array.fromList

    over (L.foo << onEveryIx << snd << L.bar) ((+) 1) arrayRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}] |> Array.fromList}

-}
onEveryIx : Relation ( Int, attribute ) reachable built -> Relation (Array attribute) reachable (Array built)
onEveryIx =
    makeOneToN_ "#[]"
        (\fn ->
            Array.indexedMap
                (\idx -> Tuple.pair idx >> fn)
        )
        (\fn ->
            Array.indexedMap
                (\idx -> Tuple.pair idx >> fn >> Tuple.second)
        )


{-| This accessor combinator lets you access values inside Maybe.

    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Lens as L

    maybeRecord : { foo : Maybe { bar : Int }, qux : Maybe { bar : Int } }
    maybeRecord = { foo = Just { bar = 2 }
                  , qux = Nothing
                  }

    get (L.foo << try << L.bar) maybeRecord
    --> Just 2

    get (L.qux << try << L.bar) maybeRecord
    --> Nothing

    over (L.foo << try << L.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 3}, qux = Nothing}

    over (L.qux << try << L.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 2}, qux = Nothing}

-}
try : Relation attribute built transformed -> Relation (Maybe attribute) built (Maybe transformed)
try =
    makeOneToN_ "?" Maybe.map Maybe.map


{-| This accessor combinator lets you provide a default value for otherwise failable compositions

    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Dict exposing (Dict)
    import Lens as L

    dict : Dict String {bar : Int}
    dict =
        Dict.fromList [("foo", {bar = 2})]

    get (key "foo" << def {bar = 0}) dict
    --> {bar = 2}

    get (key "baz" << def {bar = 0}) dict
    --> {bar = 0}

    -- NOTE: The following do not compile :thinking:
    --get (key "foo" << try << L.bar << def 0) dict
    ----> 2

    --get (key "baz" << try << L.bar << def 0) dict
    ----> 0

-}
def : attribute -> Relation attribute reachable wrap -> Relation (Maybe attribute) reachable wrap
def d =
    makeOneToN_ "??"
        (\f -> Maybe.withDefault d >> f)
        Maybe.map


{-| This accessor combinator lets you provide a default value for otherwise failable compositions

    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Dict exposing (Dict)
    import Lens as L

    dict : Dict String {bar : Int}
    dict =
        Dict.fromList [("foo", {bar = 2})]

    -- NOTE: Use `def` for this.
    --get (key "foo" << or {bar = 0}) dict
    ----> {bar = 2}

    --get (key "baz" << or {bar = 0}) dict
    ----> {bar = 0}

    get ((key "foo" << try << L.bar) |> or 0) dict
    --> 2

    get ((key "baz" << try << L.bar) |> or 0) dict
    --> 0

-}
or :
    attribute
    -> (Relation attribute attribute attribute -> Relation structure attribute (Maybe attribute))
    -> (Relation attribute other attribute -> Relation structure other attribute)
or d l =
    makeOneToOne_ "||"
        (get l >> Maybe.withDefault d)
        (over l)


{-| values: This accessor lets you traverse a Dict including the index of each element

    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Lens as L
    import Dict exposing (Dict)

    dictRecord : {foo : Dict String {bar : Int}}
    dictRecord = { foo = [ ("a", { bar = 2 })
                         , ("b", { bar = 3 })
                         , ("c", { bar = 4 })
                         ] |> Dict.fromList
                 }

    get (L.foo << values) dictRecord
    --> [("a", {bar = 2}), ("b", {bar = 3}), ("c", {bar = 4})] |> Dict.fromList

    over (L.foo << values << L.bar) ((*) 10) dictRecord
    --> {foo = [("a", {bar = 20}), ("b", {bar = 30}), ("c", {bar = 40})] |> Dict.fromList}

    get (L.foo << values << L.bar) dictRecord
    --> [("a", 2), ("b", 3), ("c", 4)] |> Dict.fromList

    over (L.foo << values << L.bar) ((+) 1) dictRecord
    --> {foo = [("a", {bar = 3}), ("b", {bar = 4}), ("c", {bar = 5})] |> Dict.fromList}

-}
values : Relation attribute reachable built -> Relation (Dict comparable attribute) reachable (Dict comparable built)
values =
    makeOneToN_ "{_}"
        (\fn -> Dict.map (\_ -> fn))
        (\fn -> Dict.map (\_ -> fn))


{-| keyed: This accessor lets you traverse a Dict including the index of each element

    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Lens as L
    import Dict exposing (Dict)

    dictRecord : {foo : Dict String {bar : Int}}
    dictRecord = { foo = [ ("a", { bar = 2 })
                         , ("b", { bar = 3 })
                         , ("c", { bar = 4 })
                         ] |> Dict.fromList
                 }

    multiplyIfA : (String, { bar : Int }) -> (String, { bar : Int })
    multiplyIfA ( key, ({ bar } as rec) ) =
        if key == "a" then
            ( key, { bar = bar * 10 } )
        else
            (key, rec)


    get (L.foo << keyed) dictRecord
    --> [("a", ("a", {bar = 2})), ("b", ("b", {bar = 3})), ("c", ("c", {bar = 4}))] |> Dict.fromList

    over (L.foo << keyed) multiplyIfA dictRecord
    --> {foo = [("a", {bar = 20}), ("b", {bar = 3}), ("c", {bar = 4})] |> Dict.fromList}

    get (L.foo << keyed << snd << L.bar) dictRecord
    --> [("a", 2), ("b", 3), ("c", 4)] |> Dict.fromList

    over (L.foo << keyed << snd << L.bar) ((+) 1) dictRecord
    --> {foo = [("a", {bar = 3}), ("b", {bar = 4}), ("c", {bar = 5})] |> Dict.fromList}

-}
keyed : Relation ( comparable, attribute ) reachable built -> Relation (Dict comparable attribute) reachable (Dict comparable built)
keyed =
    makeOneToN_ "{_}"
        (\fn -> Dict.map (\idx -> Tuple.pair idx >> fn))
        (\fn -> Dict.map (\idx -> Tuple.pair idx >> fn >> Tuple.second))


{-| key: NON-structure preserving accessor over Dict's

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Lens as L

    dict : Dict String {bar : Int}
    dict = Dict.fromList [("foo", {bar = 2})]

    get (key "foo") dict
    --> Just {bar = 2}

    get (key "baz") dict
    --> Nothing

    get (key "foo" << try << L.bar) dict
    --> Just 2

    set (key "foo") Nothing dict
    --> Dict.remove "foo" dict

    set (key "baz" << try << L.bar) 3 dict
    --> dict

-}
key : comparable -> Relation (Maybe attribute) reachable wrap -> Relation (Dict comparable attribute) reachable wrap
key k =
    makeOneToOne_ "{}" (Dict.get k) (Dict.update k)


{-| Alias for backwards compatibility
-}
dictEntry : comparable -> Relation (Maybe v) reachable wrap -> Relation (Dict comparable v) reachable wrap
dictEntry =
    key


{-| at: Structure Preserving accessor over List members.

    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Lens as L

    list : List { bar : String }
    list = [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    get (at 1) list
    --> Just { bar = "Things" }

    get (at 9000) list
    --> Nothing

    get (at 0 << L.bar) list
    --> Just "Stuff"

    set (at 0 << L.bar) "Whatever" list
    --> [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (at 9000 << L.bar) "Whatever" list
    --> list

-}
at : Int -> Relation v reachable wrap -> Relation (List v) reachable (Maybe wrap)
at idx =
    makeOneToOne_ ("(" ++ String.fromInt idx ++ ")")
        (if idx < 0 then
            always Nothing

         else
            List.head << List.drop idx
        )
        (\fn ->
            -- NOTE: `<< try` at the end ensures we can't delete any existing keys
            -- so `List.filterMap identity` should be safe
            -- TODO: write this in terms of `foldr` to avoid double iteration.
            List.indexedMap
                (\idx_ v ->
                    if idx == idx_ then
                        fn (Just v)

                    else
                        Just v
                )
                >> List.filterMap identity
        )
        << try


{-| This accessor combinator lets you access Dict members.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Array exposing (Array)
    import Accessors exposing (..)
    import Accessors.Library exposing (..)
    import Lens as L

    arr : Array { bar : String }
    arr = Array.fromList [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    get (ix 1) arr
    --> Just { bar = "Things" }

    get (ix 9000) arr
    --> Nothing

    get (ix 0 << L.bar) arr
    --> Just "Stuff"

    set (ix 0 << L.bar) "Whatever" arr
    --> Array.fromList [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (ix 9000 << L.bar) "Whatever" arr
    --> arr

-}
ix : Int -> Relation v reachable wrap -> Relation (Array v) reachable (Maybe wrap)
ix idx =
    makeOneToOne_
        ("[" ++ String.fromInt idx ++ "]")
        (Array.get idx)
        (\fn ->
            -- NOTE: `<< try` at the end ensures we can't delete any existing keys
            -- so `List.filterMap identity` should be safe
            -- TODO: there's a better way to write this no doubt.
            Array.indexedMap
                (\idx_ v ->
                    if idx == idx_ then
                        fn (Just v)

                    else
                        Just v
                )
                >> Array.foldl
                    (\e acc ->
                        case e of
                            Just v ->
                                Array.push v acc

                            Nothing ->
                                acc
                    )
                    Array.empty
        )
        << try


{-| Lens over the first component of a Tuple

    import Accessors exposing (..)
    import Accessors.Library exposing (..)

    charging : (String, Int)
    charging = ("It's over", 1)

    get fst charging
    --> "It's over"

    set fst "It's over" charging
    --> ("It's over", 1)

    over fst (\s -> String.toUpper s ++ "!!!") charging
    --> ("IT'S OVER!!!", 1)

-}
fst : Relation sub reachable wrap -> Relation ( sub, x ) reachable wrap
fst =
    makeOneToOne_ "_1" Tuple.first Tuple.mapFirst


{-|

    import Accessors exposing (..)
    import Accessors.Library exposing (..)

    meh : (String, Int)
    meh = ("It's over", 1)

    get snd meh
    --> 1

    set snd 1125 meh
    --> ("It's over", 1125)

    meh
        |> set snd 1125
        |> over fst (\s -> String.toUpper s ++ "!!!")
        |> over snd ((*) 8)
    --> ("IT'S OVER!!!", 9000)

-}
snd : Relation sub reachable wrap -> Relation ( x, sub ) reachable wrap
snd =
    makeOneToOne_ "_2" Tuple.second Tuple.mapSecond
