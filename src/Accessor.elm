module Accessor exposing
    ( Relation, Lens
    , view, is
    , description, descriptionToString
    , map, mapLazy
    , onJust, valueElseOnNothing
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

@docs Relation, Lens


## scan

@docs view, is
@docs description, descriptionToString


## nested map

@docs map, mapLazy


## for `Maybe`

@docs onJust, valueElseOnNothing


## for `Result`

@docs onOk, onErr


# Build your own accessors

Accessors are built using these functions:

@docs for1To1, for1ToN

-}

import Array exposing (Array)
import Dict exposing (Dict)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| Intuitively, a "Lens" type could look like

    type alias Lens structure focus =
        { view : structure -> focus
        , replace : focus -> (structure -> structure)
        }

Unfortunately, we then need `composeLens`, `composeIso`, `composePrism` functions.

With this approach we're able to make use of `>>`
to [`view`](#view)/[`map`](#map) a nested structure.

Technical note: This is an approximation of [Van Laarhoven encoded lenses](https://www.tweag.io/blog/2022-05-05-existential-optics/).

-}
type alias Lens structure focusView focus focusFocus =
    Relation focus focusFocus focusView
    -> Relation structure focusFocus focusView


{-| Simplified version of [`Lens`](#Lens)
which breaks type inference when used in complex compositions.
-}
type alias LensFinal structure focus =
    Lens structure focus focus focus



-- type alias Getable structure transformed attribute built reachable =
--     Relation attribute built attribute
--     -> Relation structure reachable transformed
-- type alias Watami structure transformed attribute built =
--     Relation attribute (Maybe built) transformed
--     -> Relation structure (Maybe built) (Maybe transformed)


{-| A `Relation structure focus accessible` describes how to interact with a
`focus` when given a `structure`.

Sometimes, `view` can't return a `focus`
For instance, `List focus` may not actually contain 1 `focus`.
Therefore, `accessible` can be a simple wrapper which, in that example, will be `List focus`

-}
type Relation structure focus accessible
    = Relation
        { view : structure -> accessible
        , map : (focus -> focus) -> (structure -> structure)
        , description : List Description
        }


{-| takes

  - An accessor
  - A datastructure with type `super`

and returns the value accessed by that combinator.

    { foo = { bar = "filling } } |> view (foo << bar)
    --→ "filling"

-}
view :
    (Relation focus focus focus
     -> Relation structure focus accessible
    )
    ->
        (structure
         -> accessible
        )
view accessor =
    \structure ->
        let
            (Relation relation) =
                accessor same
        in
        relation.view structure


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
    (Relation focus focus focus
     -> Relation structure focus (Maybe focusTransformed)
    )
    ->
        (structure
         -> Bool
        )
is prism =
    \structure ->
        (structure |> view prism) /= Nothing


description :
    (Relation focus focus focus
     -> Relation structure focus accessible
    )
    -> List Description
description accessor =
    let
        (Relation relation) =
            accessor same
    in
    relation.description


type Description
    = Identity
    | FocusDeeper
        { structure : String
        , focus : String
        }


{-| This function gives the name of the composition of accessors as a string.
This is useful when you want to use type safe composition of functions as an identifier
similar to the way you'd use a Sum type's constructors to key a dictionary for a form.

    import Accessors exposing (name)
    import Dict.Accessor as Dict
    import Field

    name (Field.email << onJust << Field.info << Dict.valueAtString "subject")
    --> ".email>Maybe.Just>.info>Dict value at \"subject\""

-}
descriptionToString : List Description -> String
descriptionToString =
    \descriptionsNested ->
        descriptionsNested
            |> List.filterMap
                (\description_ ->
                    case description_ of
                        Identity ->
                            Nothing

                        FocusDeeper accessing ->
                            (accessing.structure ++ ">" ++ accessing.focus) |> Just
                )
            |> String.join ":"


same : Relation focus focus focus
same =
    Relation
        { description = [ Identity ]
        , view = identity
        , map = identity
        }


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    foo : Relation field sub wrap -> Relation { record | foo : field } sub wrap
    foo =
        for1To1
            { description = { structure = "record", focus = ".foo" }
            , view = .foo
            , map = \alter record -> { record | foo = record.foo |> alter }
            }

-}
for1To1 :
    { description :
        { structure : String
        , focus : String
        }
    , view : structure -> focus
    , map : (focus -> focus) -> structure -> structure
    }
    -> (Relation focus reachable wrap -> Relation structure reachable wrap)
for1To1 config =
    \(Relation focus) ->
        Relation
            { view =
                \structure -> focus.view (config.view structure)
            , map =
                \change structure -> config.map (focus.map change) structure
            , description =
                focus.description |> (::) (FocusDeeper config.description)
            }


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    each : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
    each =
        for1ToN
            { description = { structure = "List", focus = "element each" }
            , view = List.map
            , map = List.map
            }

-}
for1ToN :
    { description :
        { structure : String
        , focus : String
        }
    , view :
        (focus -> focusFocusAccessible) -> (structure -> focusView)
    , map :
        (focus -> focus) -> (structure -> structure)
    }
    ->
        (Relation focus focusFocus focusFocusAccessible
         -> Relation structure focusFocus focusView
        )
for1ToN config =
    \(Relation sub) ->
        Relation
            { view =
                \super -> config.view sub.view super
            , map =
                \change super -> config.map (sub.map change) super
            , description =
                sub.description |> (::) (FocusDeeper config.description)
            }


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
    (Relation focus focus focus -> Relation structure focus transformed)
    -> (focus -> focus)
    ->
        (structure
         -> structure
        )
map accessor change =
    let
        (Relation relation) =
            accessor same
    in
    relation.map change


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

    mapLazy (foo << qux) ((+) 1) myRecord

-}
mapLazy :
    (Relation focus focus focus -> Relation structure focus wrap)
    -> (focus -> focus)
    ->
        (structure
         -> structure
        )
mapLazy accessor change =
    \structure ->
        let
            newSuper =
                map accessor change structure
        in
        if
            view accessor newSuper
                /= view accessor structure
        then
            newSuper

        else
            structure


{-| This accessor combinator lets you view values inside Maybe.

    import Accessors exposing (view, map, try)
    import Field

    maybeRecord : { foo : Maybe { bar : Int }, qux : Maybe { bar : Int } }
    maybeRecord =
        { foo = Just { bar = 2 }
        , qux = Nothing
        }

    view (Field.foo << onJust << Field.bar) maybeRecord
    --> Just 2

    view (Field.qux << onJust << Field.bar) maybeRecord
    --> Nothing

    map (Field.foo << onJust << Field.bar) ((+) 1) maybeRecord
    --> { foo = Just { bar = 3 }, qux = Nothing }

    map (Field.qux << onJust << Field.bar) ((+) 1) maybeRecord
    --> { foo = Just { bar = 2 }, qux = Nothing }

-}
onJust : Relation attribute built transformed -> Relation (Maybe attribute) built (Maybe transformed)
onJust =
    for1ToN
        { description = { structure = "Maybe", focus = "Just" }
        , view = Maybe.map
        , map = Maybe.map
        }


{-| Provide a default value for otherwise fallible compositions

    import Dict exposing (Dict)
    import Field
    import Dict.Accessor as Dict

    dict : Dict String { bar : Int }
    dict =
        Dict.fromList [ ( "foo", { bar = 2 } ) ]

    view (Dict.atValueString "foo" << valueElseOnNothing { bar = 0 }) dict
    --> { bar = 2 }

    view (Dict.atValueString "baz" << valueElseOnNothing { bar = 0 }) dict
    --> { bar = 0 }

TODO: The following do not compile :thinking:

    dict
        |> view
            (Dict.atValueString "foo"
                << onJust
                << Field.bar
                << valueElseOnNothing 0
            )
    ----> 2

    dict
        |> view
            (Dict.atValueString "baz"
                << onJust
                << Field.bar
                << valueElseOnNothing 0
            )
    ----> 0

-}
valueElseOnNothing : attribute -> Relation attribute reachable wrap -> Relation (Maybe attribute) reachable wrap
valueElseOnNothing fallback =
    for1ToN
        { description = { structure = "Maybe", focus = "Nothing" }
        , view = \f -> Maybe.withDefault fallback >> f
        , map = Maybe.map
        }


{-| This accessor lets you view values inside the Ok variant of a Result.

    import Accessors exposing (view, map, onOk)
    import Field

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord =
        { foo = Ok { bar = 2 }
        , qux = Err "Not an Int"
        }

    maybeRecord |> view (Field.foo << onOk << Field.bar)
    --> Just 2

    maybeRecord |> view (Field.qux << onOk << Field.bar)
    --> Nothing

    maybeRecord |> map (Field.foo << onOk << Field.bar) ((+) 1)
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    maybeRecord |> map (Field.qux << onOk << Field.bar) ((+) 1)
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
onOk : Relation attribute built transformed -> Relation (Result x attribute) built (Maybe transformed)
onOk =
    for1ToN
        { description = { structure = "Result", focus = "Ok" }
        , view = \fn -> Result.map fn >> Result.toMaybe
        , map = Result.map
        }


{-| This accessor lets you view values inside the Err variant of a Result.

    import Accessors exposing (view, map, onErr)
    import Field

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord =
        { foo = Ok { bar = 2 }
        , qux = Err "Not an Int"
        }

    maybeRecord |> view (Field.foo << onErr)
    --> Nothing

    maybeRecord |> view (Field.qux << onErr)
    --> Just "Not an Int"

    maybeRecord |> map (Field.foo << onErr) String.toUpper
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    maybeRecord |> map (Field.qux << onErr) String.toUpper
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
        , view = accessing
        , map = Result.mapError
        }
