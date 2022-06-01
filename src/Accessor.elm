module Accessor exposing
    ( Relation, Accessor, Lens, LensArgument
    , access, is
    , description, descriptionToString
    , map, overLazy
    , onJust, def, or
    , onOk, onErr
    , for1To1, for1ToN
    )

{-| Relations are interfaces to document the relation between two data
structures. For convenience, we'll call the containing structure `super`, and
the contained structure `sub`. What a `Relation` claims is that a `super` is
referencing a `sub` in some way.

Relations are the building blocks of accessors. An accessor is a function that
expects a `Relation` and builds a new relation with it. Accessors are
composable, which means you can build a chain of relations to manipulate nested
structures without handling the packing and the unpacking.


# Relation

@docs Relation, Accessor, Lens, LensArgument


## scan

@docs access, is
@docs description, descriptionToString


## nested map

@docs map, overLazy


## for `Maybe`

@docs onJust, def, or


## for `Result`

@docs onOk, onErr


# Build your own accessors

Accessors are built using these functions:

@docs for1To1, for1ToN

-}

import Array exposing (Array)
import Dict exposing (Dict)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| The most general version of this type that everything else specializes
-}
type alias Accessor dataBefore dataAfter attrBefore attrAfter reachable =
    Relation attrBefore reachable attrAfter -> Relation dataBefore reachable dataAfter


{-| This is an approximation of Van Laarhoven encoded lenses which enable the
the callers to use regular function composition to build more complex nested
updates of more complicated types.

The original "Lens" type looked more like

    type alias Lens structure focus =
        { access : structure -> focus
        , set : structure -> focus -> structure
        }

Unfortunately, these can't be composed without
defining custom `composeLens`, `composeIso`, `composePrism` functions.

With this approach we're able to make use of `<<`
to [`access`](#access)/[`map`](#map) deeply nested data.

-}
type alias
    Lens
        -- Structure Before Action
        structure
        -- Structure After Action
        structureTransformed
        -- Focus Before action
        focus
        -- Focus After action
        focusTransformed
    =
    Relation focus focusTransformed structureTransformed
    -> Relation structure focusTransformed structureTransformed


{-| Simplified version of Lens but seems to break type inference for more complicated compositions.
-}
type alias LensArgument structure attribute =
    Lens structure attribute attribute attribute



-- type alias Getable structure transformed attribute built reachable =
--     Relation attribute built attribute
--     -> Relation structure reachable transformed
-- type alias Watami structure transformed attribute built =
--     Relation attribute (Maybe built) transformed
--     -> Relation structure (Maybe built) (Maybe transformed)


{-| A `Relation super sub wrap` is a type describing how to interact with a
`sub` data when given a `super` data.

The `wrap` exists because some types can't ensure that `access` will return a
`sub`. For instance, `Maybe sub` may not actually contain a `sub`. Therefore,
`access` returns a `wrap` which, in that example, will be `Maybe sub`

Implementation: A relation is a banal record storing a `access` function and an
`map` function.

-}
type Relation structure attribute wrap
    = Relation
        { access : structure -> wrap
        , map : (attribute -> attribute) -> (structure -> structure)
        , description : List Description
        }


{-| takes

  - An accessor
  - A datastructure with type `super`

and returns the value accessed by that combinator.

    { foo = { bar = "filling } } |> access (foo << bar)
    --→ "filling"

-}
access :
    (Relation attribute built attribute -> Relation structure reachable transformed)
    -> structure
    -> transformed
access accessor =
    \structure ->
        let
            (Relation relation) =
                accessor
                    (Relation
                        { access = \super -> super
                        , map = void
                        , description = []
                        }
                    )
        in
        relation.access structure


void : a -> b
void super =
    void super


{-| This function gives the name of the composition of accessors as a string.
This is useful when you want to use type safe composition of functions as an identifier
similar to the way you'd use a Sum type's constructors to key a dictionary for a form.

    import Accessors exposing (name)
    import Dict.Accessor as Dict
    import Lens as L

    name (L.email << onJust << L.info << Dict.valueAtString "subject")
    --> ".email>Maybe.Just>.info>Dict value at \"subject\""

-}
descriptionToString : List Description -> String
descriptionToString =
    \descriptionsNested ->
        descriptionsNested
            |> List.map
                (\description_ ->
                    description_.structure ++ ">" ++ description_.focus
                )
            |> String.join ":"



-- type alias Modifiable =
--    Relation attribute x y -> Relation structure a transformed


{-| The map function takes:

  - An accessor,
  - A function `(sub -> sub)`,
  - A datastructure with type `super`
    and it returns the data structure, with the accessible field changed by applying
    the function to the existing value.

```
map (foo << qux) ((+) 1) myRecord
```

-}
map :
    (Relation attribute attribute built -> Relation structure attribute transformed)
    -> (attribute -> attribute)
    -> structure
    -> structure
map accessor change s =
    let
        (Relation relation) =
            accessor
                (Relation
                    { access = void
                    , map = \fn -> fn
                    , description = []
                    }
                )
    in
    relation.map change s


{-| Lazy versions of [`map`](#map).

These actions check that the old and the new version are different before writing.
They are useful when used together with `Html.lazy`, because it uses reference
equality for complex structures. Therefore, using lazy `map` will
not prevent `Html.lazy` from doing its work.

The map function takes:

  - An accessor,
  - A function `(sub -> sub)`,
  - A datastructure with type `super`

and it returns the data structure, with the accessible field changed by applying
the function to the existing value.
The structure is changed only if the new field is different from the old one.

    overLazy (foo << qux) ((+) 1) myRecord

-}
overLazy :
    (Relation sub sub sub -> Relation super sub wrap)
    -> (sub -> sub)
    -> super
    -> super
overLazy accessor change =
    \structure ->
        let
            newSuper =
                map accessor change structure
        in
        if
            access accessor newSuper
                /= access accessor structure
        then
            newSuper

        else
            structure


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    foo : Relation field sub wrap -> Relation { record | foo : field } sub wrap
    foo =
        for1To1
            { description = { structure = "record", focus = ".foo" }
            , access = .foo
            , map = \alter record -> { record | foo = record.foo |> alter }
            }

-}
for1To1 :
    { description : Description
    , access : structure -> focus
    , map : (focus -> focus) -> structure -> structure
    }
    -> (Relation focus reachable wrap -> Relation structure reachable wrap)
for1To1 config =
    \(Relation focus) ->
        Relation
            { access =
                \structure -> focus.access (config.access structure)
            , map =
                \change structure -> config.map (focus.map change) structure
            , description =
                focus.description |> (::) config.description
            }


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    each : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
    each =
        for1ToN
            { description = { structure = "List", focus = "element each" }
            , access = List.map
            , map = List.map
            }

-}
for1ToN :
    { description : Description
    , access :
        (attribute -> built) -> structure -> transformed
    , map :
        (attribute -> attribute) -> structure -> structure
    }
    ->
        (-- What is reachable here?
         Relation attribute reachable built
         -> Relation structure reachable transformed
        )
for1ToN config =
    \(Relation sub) ->
        Relation
            { access =
                \super -> config.access sub.access super
            , map =
                \change super -> config.map (sub.map change) super
            , description =
                sub.description |> (::) config.description
            }


description :
    Accessor dataBefore dataAfter attrBefore attrAfter reachable
    -> List Description
description accessor =
    let
        (Relation relation) =
            accessor
                (Relation
                    -- can we a-void this?
                    { access = void
                    , map = void
                    , description = []
                    }
                )
    in
    relation.description


type alias Description =
    RecordWithoutConstructorFunction
        { structure : String
        , focus : String
        }


{-| This accessor combinator lets you access values inside Maybe.

    import Accessors exposing (access, map, try)
    import Lens as L

    maybeRecord : { foo : Maybe { bar : Int }, qux : Maybe { bar : Int } }
    maybeRecord =
        { foo = Just { bar = 2 }
        , qux = Nothing
        }

    access (L.foo << onJust << L.bar) maybeRecord
    --> Just 2

    access (L.qux << onJust << L.bar) maybeRecord
    --> Nothing

    map (L.foo << onJust << L.bar) ((+) 1) maybeRecord
    --> { foo = Just { bar = 3 }, qux = Nothing }

    map (L.qux << onJust << L.bar) ((+) 1) maybeRecord
    --> { foo = Just { bar = 2 }, qux = Nothing }

-}
onJust : Relation attribute built transformed -> Relation (Maybe attribute) built (Maybe transformed)
onJust =
    for1ToN
        { description = { structure = "Maybe", focus = "Just" }
        , access = Maybe.map
        , map = Maybe.map
        }


{-| Provide a default value for otherwise fallible compositions

    import Dict exposing (Dict)
    import Lens as L

    dict : Dict String { bar : Int }
    dict =
        Dict.fromList [ ( "foo", { bar = 2 } ) ]

    access (key "foo" << def { bar = 0 }) dict
    --> { bar = 2 }

    access (key "baz" << def { bar = 0 }) dict
    --> { bar = 0 }

TODO: The following do not compile :thinking:

    dict |> access (key "foo" << onJust << L.bar << def 0)
    ----> 2

    dict |> access (key "baz" << onJust << L.bar << def 0)
    ----> 0

-}
def : attribute -> Relation attribute reachable wrap -> Relation (Maybe attribute) reachable wrap
def fallback =
    for1ToN
        { description = { structure = "Maybe", focus = "Nothing" }
        , access = \f -> Maybe.withDefault fallback >> f
        , map = Maybe.map
        }


{-| This accessor combinator lets you provide a fallback value for fallible compositions

    import Dict exposing (Dict)
    import Lens as L

    dict : Dict String { bar : Int }
    dict =
        Dict.fromList [ ( "foo", { bar = 2 } ) ]

NOTE: Use `def` for this

    dict |> access (key "foo" << or { bar = 0 })
    ----> { bar = 2 }

    dict |> access (key "baz" << or { bar = 0 })
    ----> { bar = 0 }

    dict |> access ((key "foo" << onJust << L.bar) |> or 0)
    --> 2

    dict |> access ((key "baz" << onJust << L.bar) |> or 0)
    --> 0

-}
or :
    attribute
    -> (Relation attribute attribute attribute -> Relation structure attribute (Maybe attribute))
    -> (Relation attribute other attribute -> Relation structure other attribute)
or fallback =
    \l ->
        for1To1
            { description = { structure = "Maybe", focus = "Nothing" }
            , access = access l >> Maybe.withDefault fallback
            , map = map l
            }


{-| This accessor lets you access values inside the Ok variant of a Result.

    import Accessors exposing (access, map, onOk)
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord =
        { foo = Ok { bar = 2 }
        , qux = Err "Not an Int"
        }

    maybeRecord |> access (L.foo << onOk << L.bar)
    --> Just 2

    maybeRecord |> access (L.qux << onOk << L.bar)
    --> Nothing

    maybeRecord |> map (L.foo << onOk << L.bar) ((+) 1)
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    maybeRecord |> map (L.qux << onOk << L.bar) ((+) 1)
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
onOk : Relation attribute built transformed -> Relation (Result x attribute) built (Maybe transformed)
onOk =
    for1ToN
        { description = { structure = "Result", focus = "Ok" }
        , access = \fn -> Result.map fn >> Result.toMaybe
        , map = Result.map
        }


{-| This accessor lets you access values inside the Err variant of a Result.

    import Accessors exposing (access, map, onErr)
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord =
        { foo = Ok { bar = 2 }
        , qux = Err "Not an Int"
        }

    maybeRecord |> access (L.foo << onErr)
    --> Nothing

    maybeRecord |> access (L.qux << onErr)
    --> Just "Not an Int"

    maybeRecord |> map (L.foo << onErr) String.toUpper
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    maybeRecord |> map (L.qux << onErr) String.toUpper
    --> { foo = Ok { bar = 2 }, qux = Err "NOT AN INT" }

-}
onErr : Relation attribute built transformed -> Relation (Result attribute x) built (Maybe transformed)
onErr =
    let
        accessing alter =
            \res ->
                case res of
                    Err e ->
                        e |> alter |> Just

                    _ ->
                        Nothing
    in
    for1ToN
        { description = { structure = "Result", focus = "Err" }
        , access = accessing
        , map = Result.mapError
        }


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type-safe.

    Just 1234
        |> is try
    --> True

    Nothing
        |> is try
    --> False

    ["Stuff", "things"]
        |> is (at 2)
    --> False

    ["Stuff", "things"]
        |> is (at 0)
    --> True

-}
is :
    (Relation attribute built attribute -> Relation structure reachable (Maybe transformed))
    -> structure
    -> Bool
is prism sup =
    access prism sup /= Nothing
