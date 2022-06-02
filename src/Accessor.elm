module Accessor exposing
    ( Relation, Accessor, Prism, Lens
    , create1To1, create1ToN
    , view, is
    , Description(..), description, descriptionToString
    , mapOver, mapOverLazy
    , onJust, valueElseOnNothing
    , onOk, onErr
    )

{-| Relations are interfaces to document the relation between two data
structures. For convenience, we'll call the containing structure `super`, and
the contained structure `sub`. What a `Relation` claims is that a `super` is
referencing a `sub` in some way.

Relations are the building blocks of accessors. An accessor is a function that
expects a `Relation` and builds a new relation with it. Accessors are
composable, which means you can build a chain of relations to manipulate nested
structures without handling the packing and the unpacking.

@docs Relation, Accessor, Prism, Lens


## create

@docs create1To1, create1ToN


## scan

@docs view, is
@docs Description, description, descriptionToString


## nested map

@docs mapOver, mapOverLazy


## for `Maybe`

@docs onJust, valueElseOnNothing


## for `Result`

@docs onOk, onErr

-}

import Array exposing (Array)
import Dict exposing (Dict)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| A `Relation structure focus focusView` describes how to interact with a
`focus` when given a `structure`.

Sometimes, `view` can't return a `focus`
For instance, `List focus` may not actually contain 1 `focus`.
Therefore, `focusView` can be a simple wrapper which, in that example, will be `List focus`

-}
type Relation structure focus focusView
    = Relation
        { view : structure -> focusView
        , map : (focus -> focus) -> (structure -> structure)
        , description : List Description
        }


type alias Accessor structure focus focusView focusFocus focusFocusView =
    Relation focus focusFocus focusFocusView
    -> Relation structure focusFocus focusView


{-| Intuitively, a "Lens" type could look like

    type alias Lens structure focus =
        { view : structure -> focus
        , replace : focus -> (structure -> structure)
        }

Unfortunately, we then need `composeLens`, `composeIso`, `composePrism` functions.

With this approach we're able to make use of `<<`
to [`view`](#view)/[`map`](#map) a nested structure.

Technical note: This is an approximation of [Van Laarhoven encoded lenses](https://www.tweag.io/blog/2022-05-05-existential-optics/).

-}
type alias Lens structure focus focusFocus focusFocusView =
    Accessor structure focus focusFocusView focusFocus focusFocusView


{-| Traversal over a single value that might or might not exist. Examples

  - [`onJust`](#onJust)
  - [`onOk`](#onOk)
  - [`onErr`](#onErr)

-}
type alias Prism structure value focusFocus valueView =
    Relation value focusFocus valueView -> Relation structure focusFocus (Maybe valueView)


{-| takes

  - An accessor
  - A datastructure with type `super`

and returns the value accessed by that combinator.

    { foo = { bar = "filling } } |> view (foo << bar)
    --→ "filling"

-}
view :
    (Relation focus focus focus
     -> Relation structure focus focusView
    )
    ->
        (structure
         -> focusView
        )
view accessor =
    let
        (Relation relation) =
            accessor same
    in
    relation.view


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type-safe.

    Just 1234
        |> is try
    --> True

    Nothing
        |> is try
    --> False

    [ "Stuff", "things" ]
        |> is (at 2)
    --> False

    [ "Stuff", "things" ]
        |> is (at 0)
    --> True

-}
is :
    (Relation (Maybe value) (Maybe value) (Maybe value)
     -> Relation structure (Maybe value) (Maybe valueView)
    )
    ->
        (structure
         -> Bool
        )
is prism =
    \structure ->
        (structure |> view prism) /= Nothing


{-| Each `Relation` has a [`description`](#description) to get unique names out of compositions of accessors.
This is useful when you want type-safe keys for a `Dict` but you still want to use the `elm/core` implementation.
-}
description :
    (Relation focus focus focus
     -> Relation structure focus focusView
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


same : Relation structure structure structure
same =
    Relation
        { description = [ Identity ]
        , view = identity
        , map = identity
        }


{-| Create a [`Lens`](#Lens) from

  - describing the structure and the targeted focus
  - a function to [access](#view) the structure's targeted focus
  - a function on the structure for mapping the targeted focus

```
foo : Lens { record | foo : foo } foo focusFocus focusFocusView
foo =
    create1To1
        { description = { structure = "record", focus = ".foo" }
        , view = .foo
        , map = \alter record -> { record | foo = record.foo |> alter }
        }
```

-}
create1To1 :
    { description :
        { structure : String
        , focus : String
        }
    , view : structure -> focus
    , map : (focus -> focus) -> (structure -> structure)
    }
    -> Lens structure focus focusFocus focusFocusView
create1To1 focus =
    \(Relation deeperFocus) ->
        Relation
            { view =
                \structure -> structure |> focus.view |> deeperFocus.view
            , map =
                \change -> focus.map (deeperFocus.map change)
            , description =
                deeperFocus.description
                    |> (::) (FocusDeeper focus.description)
            }


{-| Create a traversal [`Accessor`](#Accessor) from

  - describing the structure and the targeted focus
  - a function on the structure for mapping the targeted focus [`view`](#view)
  - a function on the structure for mapping the targeted focus

```
elementEach : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
elementEach =
    create1ToN
        { description = { structure = "List", focus = "element each" }
        , view = List.map
        , map = List.map
        }
```

-}
create1ToN :
    { view : (focus -> focusFocusView) -> (structure -> focusView)
    , map : (focus -> focus) -> (structure -> structure)
    , description : { structure : String, focus : String }
    }
    -> Accessor structure focus focusView focusFocus focusFocusView
create1ToN focus =
    \(Relation deeperFocus) ->
        Relation
            { view =
                focus.view deeperFocus.view
            , map =
                \change ->
                    focus.map
                        (deeperFocus.map
                            change
                        )
            , description =
                deeperFocus.description
                    |> (::) (FocusDeeper focus.description)
            }


{-| The map function takes:

  - An accessor,
  - A function `(sub -> sub)`,
  - A datastructure with type `super`
    and it returns the data structure, with the focusView field changed by applying
    the function to the existing value.

```
map (foo << qux) ((+) 1) myRecord
```

-}
mapOver :
    (Relation focus focus focus
     -> Relation structure focus focusView
    )
    -> (focus -> focus)
    ->
        (structure
         -> structure
        )
mapOver accessor change =
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

  - An accessor
  - A function `(focus -> focus)`
  - A data `structure`

and it returns the data `structure`, with the focusView field changed by applying
the function to the existing value.
The structure is changed only if the new field is different from the old one.

    mapLazy (Field.foo << Field.qux) ((+) 1) myRecord

-}
mapOverLazy :
    (Relation focus focus focus
     -> Relation structure focus focusView
    )
    -> (focus -> focus)
    ->
        (structure
         -> structure
        )
mapOverLazy accessor change =
    \structure ->
        let
            changedStructure =
                structure |> mapOver accessor change
        in
        if
            (changedStructure |> view accessor)
                /= (structure |> view accessor)
        then
            changedStructure

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
onJust : Prism (Maybe value) value focusFocus valueView
onJust =
    create1ToN
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

    dict
        |> view
            (Dict.atValueString "foo"
                << onJust
                << Field.bar
                << onJust
                << valueElseOnNothing 0
            )
    ---> 2

    dict
        |> view
            (Dict.atValueString "baz"
                << onJust
                << Field.bar
                << onJust
                << valueElseOnNothing 0
            )
    ---> 0

-}
valueElseOnNothing : value -> Relation value focusFocus focusFocusView -> Relation (Maybe value) focusFocus focusFocusView
valueElseOnNothing fallback =
    create1ToN
        { description = { structure = "Maybe", focus = "Nothing" }
        , view =
            \valueMap ->
                \maybe ->
                    maybe
                        |> Maybe.withDefault fallback
                        |> valueMap
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

    maybeRecord |> mapOver (Field.foo << onOk << Field.bar) ((+) 1)
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    maybeRecord |> mapOver (Field.qux << onOk << Field.bar) ((+) 1)
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
onOk : Prism (Result error value) value focusFocus focusFocusView
onOk =
    create1ToN
        { description = { structure = "Result", focus = "Ok" }
        , view =
            \okMap ->
                \result ->
                    case result of
                        Err _ ->
                            Nothing

                        Ok ok ->
                            ok |> okMap |> Just
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

    maybeRecord |> mapOver (Field.foo << onErr) String.toUpper
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    maybeRecord |> mapOver (Field.qux << onErr) String.toUpper
    --> { foo = Ok { bar = 2 }, qux = Err "NOT AN INT" }

-}
onErr : Prism (Result error value) error focusFocus focusFocusView
onErr =
    create1ToN
        { description = { structure = "Result", focus = "Err" }
        , view =
            \errorMap ->
                \result ->
                    case result of
                        Ok _ ->
                            Nothing

                        Err error ->
                            error |> errorMap |> Just
        , map = Result.mapError
        }
