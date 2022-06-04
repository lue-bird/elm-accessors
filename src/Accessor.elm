module Accessor exposing
    ( Relation
    , Lens, Prism, Traversal
    , TraversalConsume
    , LensKeepingFocusType, PrismKeepingFocusType, TraversalKeepingFocusType
    , lens, prism, traversal
    , view, is
    , Description(..), description, descriptionToString
    , viewNamed, named
    , nameFocus
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

@docs Relation
@docs Lens, Prism, Traversal
@docs TraversalConsume


## keeping focus type

@docs LensKeepingFocusType, PrismKeepingFocusType, TraversalKeepingFocusType


## create

@docs lens, prism, traversal


## scan


### viewing

@docs view, is


### description

@docs Description, description, descriptionToString


### naming

@docs viewNamed, named


## change

@docs nameFocus


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
type Relation structure focus structureMapped focusMapped focusView focusNaming
    = Relation
        { view : structure -> focusView
        , map : (focus -> focusMapped) -> (structure -> structureMapped)
        , focusName : focusNaming
        , description : List Description
        }


{-| Every accessor defined using `Relation`s is a traversal of some kind.

  - [`Lens`](#Lens): 1:1
      - e.g. the selected element in a `SelectList`
  - [`Prism`](#Prism): 1:Maybe
      - e.g. the value of one of many variants
  - 1:n
      - e.g. each array element

-}
type alias Traversal structure focus structureMapped focusMapped focusNamed focusView focusFocus focusFocusMapped focusFocusNamed focusFocusView focusFocusFocusNamed =
    Relation
        focus
        focusFocus
        focusMapped
        focusFocusMapped
        focusFocusView
        (focusFocusFocusNamed -> focusFocusNamed)
    ->
        Relation
            structure
            focusFocus
            structureMapped
            focusFocusMapped
            focusView
            (focusFocusFocusNamed -> focusNamed)


{-| A [`Traversal`](#Traversal) where the focus can't `map` to a different type. Examples

  - traverse every even index
  - [`LensKeepingFocusType`](#LensKeepingFocusType)
  - [`PrismKeepingFocusType`](#PrismKeepingFocusType)

-}
type alias TraversalKeepingFocusType structure focus focusNamed focusView focusFocus focusFocusMapped focusFocusNamed focusFocusView focusFocusFocusNamed =
    Traversal
        structure
        focus
        structure
        focus
        focusNamed
        focusView
        focusFocus
        focusFocusMapped
        focusFocusNamed
        focusFocusView
        focusFocusFocusNamed


{-| [`Traversal`](#Traversal) over a focusName value: 1:1. Examples

  - [`Tuple.Accessor.first`](Tuple#first)
  - [`SelectList.Accessor.selected`](SelectList-Accessor#selected)
  - record .field value

Intuitively, a "Lens" type could look like

    type alias Lens structure focus =
        { access : structure -> focus
        , replace : focus -> (structure -> structure)
        }

Unfortunately, we then need `composeLens`, `composeIso`, `composePrism` functions.

Defining "Lens" in terms of `Relation`s:

    Relation focus focusFocus focusFocusView
    -> Relation structure focusFocus focusFocusView

we're able to make use of `<<`
to [`view`](#view)/[`map`](#map) a nested structure.

Technical note: This is an approximation of [Van Laarhoven encoded lenses](https://www.tweag.io/blog/2022-05-05-existential-optics/).

-}
type alias Lens structure focus structureMapped focusMapped focusNamed focusFocus focusFocusMapped focusFocusNamed focusFocusView focusFocusFocusNamed =
    Traversal
        structure
        focus
        structureMapped
        focusMapped
        focusNamed
        focusFocusView
        focusFocus
        focusFocusMapped
        focusFocusNamed
        focusFocusView
        focusFocusFocusNamed


{-| A [`Lens`](#Lens) where the focus can't `map` to a different type. Examples

  - [`Dict.Accessor.valueAt`](Dict-Accessor#valueAt)
  - `Record.field`. for `\r map -> { r | field = r.field |> map }`, `map` can't change the field's type

-}
type alias LensKeepingFocusType structure focus focusNamed focusFocus focusFocusMapped focusFocusNamed focusFocusView focusFocusFocusNamed =
    Lens
        structure
        focus
        structure
        focus
        focusNamed
        focusFocus
        focusFocusMapped
        focusFocusNamed
        focusFocusView
        focusFocusFocusNamed


{-| [`Traversal`](#Traversal) over a focus value that might or might not exist: 1:Maybe. Examples

  - [`onJust`](#onJust)
  - [`onOk`](#onOk)
  - [`onErr`](#onErr)

-}
type alias Prism structure focus structureMapped focusMapped focusNamed focusFocus focusFocusMapped focusFocusNamed focusFocusView focusFocusFocusNamed =
    Traversal
        structure
        focus
        structureMapped
        focusMapped
        focusNamed
        (Maybe focusFocusView)
        focusFocus
        focusFocusMapped
        focusFocusNamed
        focusFocusView
        focusFocusFocusNamed


{-| A [`Prism`](#Prism) where the focus can't `map` to a different type. Examples

  - [`List.Accessor.element`](List-Accessor#element)
  - [`Array.Accessor.element`](Array-Accessor#element)
  - [`Dict.Accessor.valueAt`](Dict-Accessor#valueAt)
  - `on[Variant]` where the variant values' types aren't exclusively `type` variables

-}
type alias PrismKeepingFocusType structure focus focusNamed focusFocus focusFocusMapped focusFocusNamed focusFocusView focusFocusFocusNamed =
    Prism
        structure
        focus
        structure
        focus
        focusNamed
        focusFocus
        focusFocusMapped
        focusFocusNamed
        focusFocusView
        focusFocusFocusNamed


{-| Only use `TraversalConsume` for accessor arguments that are **consumed** – used and then discarded:

    description :
        TraversalConsume structure focus focusView
        -> List Description

    view :
        TraversalConsume structure focus focusView
        -> (structure -> focusView)

    is :
        TraversalConsume structure value (Maybe valueView)
        -> (structure -> Bool)

    mapOver :
        TraversalConsume structure focus focusView
        ->
            ((focus -> focus)
             -> (structure -> structure)
            )

Use [`LensConsume`](#LensConsume) in the same context.

-}
type alias TraversalConsume structure focus structureMapped focusMapped focusView unnamed focusNamed =
    Relation focus focus focusMapped focusMapped focus (unnamed -> unnamed)
    -> Relation structure focus structureMapped focusMapped focusView (unnamed -> focusNamed)


{-| takes

  - An accessor
  - A data `structure`

and returns the focussed parts accessed by that combinator.

    import Record

    { foo = { bar = "filling } }
        |> view (Record.foo << Record.bar)
    --→ "filling"

-}
view :
    TraversalConsume structure focus structureMapped focusMapped focusView unnamed focusNamed
    -> (structure -> focusView)
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
    TraversalConsume
        structure
        value
        structureMapped
        valueMapped
        (Maybe valueView)
        unnamed
        focusNamed
    -> (structure -> Bool)
is prism_ =
    \structure ->
        (structure |> view prism_) /= Nothing


{-| [`view`](#view) the focus wrapped with a name describing the focus.

See also [`named`](#named) to wrap any value with a name describing the focus.

-}
viewNamed :
    TraversalConsume
        structure
        focus
        structureMapped
        focusMapped
        focusFocusView
        focusFocusView
        focusNamed
    -> (structure -> focusNamed)
viewNamed traversal_ =
    \structure ->
        structure
            |> view traversal_
            |> named traversal_


{-| Wrap a value with a name describing the focus.

See also [`viewNamed`](#viewNamed) to [`view`](#view) the focus wrapped with a name describing the focus.

-}
named :
    TraversalConsume
        structure
        focus
        structureMapped
        focusMapped
        focusFocusView
        unnamed
        focusNamed
    -> (unnamed -> focusNamed)
named traversal_ =
    let
        (Relation relation) =
            traversal_ same
    in
    relation.focusName


{-| Each `Relation` has a [`description`](#description) to get unique names out of compositions of accessors.
This is useful when you want type-safe keys for a `Dict` but you still want to use the `elm/core` implementation.
-}
description :
    TraversalConsume structure focus structureMapped focusMapped focusView focusName focusNamed
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
    import Record

    name (Record.email << onJust << Record.info << Dict.valueAtString "subject")
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


same : Relation structure structure structureMapped structureMapped structure (focus -> focus)
same =
    Relation
        { description = [ Identity ]
        , view = identity
        , map = identity
        , focusName = identity
        }


{-| Create a 1:1 [`Lens`](#Lens) from

  - describing the structure and the targeted focus
  - a function to [access](#view) the structure's targeted focus
  - a function on the structure for mapping the targeted focus

```
foo : Lens { record | foo : foo } foo focusFocus focusFocusView
foo =
    lens
        { description = { structure = "record", focus = ".foo" }
        , view = .foo
        , map = \alter record -> { record | foo = record.foo |> alter }
        }
```

-}
lens :
    { description :
        { structure : String
        , focus : String
        }
    , view : structure -> focus
    , map : (focus -> focusMapped) -> (structure -> structureMapped)
    , focusName : focusFocusNamed -> focusNamed
    }
    ->
        Lens
            structure
            focus
            structureMapped
            focusMapped
            focusNamed
            focusFocus
            focusFocusMapped
            focusFocusNamed
            focusFocusView
            focusFocusFocusNamed
lens focus =
    traversal
        { description = focus.description
        , view =
            \focusFocusView structure ->
                structure |> focus.view |> focusFocusView
        , map = focus.map
        , focusName = focus.focusName
        }


{-| Create a 1:Maybe [`Prism`](#Prism) from

  - describing the structure and the targeted focus
  - a function to [access](#view) the structure's targeted `Maybe` focus
  - a function on the structure for mapping the targeted focus

```
onOk : Prism (Result error value) value focusFocus focusFocusView
onOk =
    prism
        { description = { structure = "Result", focus = "Ok" }
        , view = Result.toMaybe
        , map = Result.map
        }
```

-}
prism :
    { description :
        { structure : String
        , focus : String
        }
    , view : structure -> Maybe focus
    , map : (focus -> focusMapped) -> (structure -> structureMapped)
    , focusName : focusFocusNamed -> focusNamed
    }
    ->
        Prism
            structure
            focus
            structureMapped
            focusMapped
            focusNamed
            focusFocus
            focusFocusMapped
            focusFocusNamed
            focusFocusView
            focusFocusFocusNamed
prism focus =
    traversal
        { description = focus.description
        , view =
            \focusFocusView structure ->
                structure |> focus.view |> Maybe.map focusFocusView
        , map = focus.map
        , focusName = focus.focusName
        }


{-| Provide a more detailed name of the focus.
-}
nameFocus :
    (focusNamed -> focusNamedMoreDetailed)
    ->
        (Traversal
            structure
            focus
            structureMapped
            focusMapped
            focusNamed
            focusView
            focusFocus
            focusFocusMapped
            focusFocusNamed
            focusFocusView
            focusFocusFocusNamed
         ->
            Traversal
                structure
                focus
                structureMapped
                focusMapped
                focusNamedMoreDetailed
                focusView
                focusFocus
                focusFocusMapped
                focusFocusNamed
                focusFocusView
                focusFocusFocusNamed
        )
nameFocus nameFocusDetailed =
    \traversalLessDetailed ->
        \focusFocus ->
            let
                (Relation focusRelation) =
                    traversalLessDetailed focusFocus
            in
            Relation
                { view = focusRelation.view
                , map = focusRelation.map
                , description = focusRelation.description
                , focusName =
                    \focusFocusNamed ->
                        focusFocusNamed
                            |> focusRelation.focusName
                            |> nameFocusDetailed
                }


{-| Create a 1:n traversal [`Accessor`](#Accessor) from

  - describing the structure and the targeted focus
  - a function on the structure for mapping the targeted focus [`view`](#view)
  - a function on the structure for mapping the targeted focus

```
elementEach :
    Traversal
        (List element)
        element
        (List elementFocusView)
        elementFocus
        elementFocusView
elementEach =
    traversal
        { description = { structure = "List", focus = "element each" }
        , view = List.map
        , map = List.map
        }
```

-}
traversal :
    { view : (focus -> focusFocusView) -> (structure -> focusView)
    , map : (focus -> focusMapped) -> (structure -> structureMapped)
    , description : { structure : String, focus : String }
    , focusName : focusFocusNamed -> focusNamed
    }
    ->
        Traversal
            structure
            focus
            structureMapped
            focusMapped
            focusNamed
            focusView
            focusFocus
            focusFocusMapped
            focusFocusNamed
            focusFocusView
            focusFocusFocusNamed
traversal focus =
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
            , focusName =
                \deeperFocus_ ->
                    deeperFocus_
                        |> deeperFocus.focusName
                        |> focus.focusName
            }


{-| The map function takes:

  - Any accessor
  - A function `focus -> focus`
  - A data `structure`

and returns the data `structure` with the focusView field changed by applying
the function to the existing value.

    { foo = { qux = 0 } }
        |> mapOver (Record.foo << Record.qux) ((+) 1)
    --> { foo = { qux = 1 } }

-}
mapOver :
    TraversalConsume
        structure
        focus
        structureMapped
        focusMapped
        focusView
        focusName
        focusNamed
    ->
        ((focus -> focusMapped)
         -> (structure -> structureMapped)
        )
mapOver accessor change =
    let
        (Relation relation) =
            accessor same
    in
    relation.map change


{-| Lazy version of [`mapOver`](#mapOver).

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

    mapLazy (Record.foo << Record.qux) ((+) 1) myRecord

-}
mapOverLazy :
    TraversalConsume structure focus structure focus focusView focusName focusNamed
    ->
        ((focus -> focus)
         -> (structure -> structure)
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



-- Maybe


{-| This accessor combinator lets you view values inside Maybe.

    import Accessors exposing (view, map, try)
    import Record

    maybeRecord : { foo : Maybe { bar : Int }, qux : Maybe { bar : Int } }
    maybeRecord =
        { foo = Just { bar = 2 }
        , qux = Nothing
        }

    view (Record.foo << onJust << Record.bar) maybeRecord
    --> Just 2

    view (Record.qux << onJust << Record.bar) maybeRecord
    --> Nothing

    map (Record.foo << onJust << Record.bar) ((+) 1) maybeRecord
    --> { foo = Just { bar = 3 }, qux = Nothing }

    map (Record.qux << onJust << Record.bar) ((+) 1) maybeRecord
    --> { foo = Just { bar = 2 }, qux = Nothing }

-}
onJust :
    Prism
        (Maybe value)
        value
        (Maybe valueMapped)
        valueMapped
        { just : valueFocusNamed }
        valueFocus
        focusFocusMapped
        valueFocusNamed
        valueFocusView
        valueFocusFocusNamed
onJust =
    prism
        { description = { structure = "Maybe", focus = "Just" }
        , view = identity
        , map = Maybe.map
        , focusName =
            \valueFocusNamed -> { just = valueFocusNamed }
        }


{-| Provide a default value for otherwise fallible compositions

    import Dict exposing (Dict)
    import Record
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
                << Record.bar
                << onJust
                << valueElseOnNothing 0
            )
    --> 2

    dict
        |> view
            (Dict.atValueString "baz"
                << onJust
                << Record.bar
                << onJust
                << valueElseOnNothing 0
            )
    --> 0

-}
valueElseOnNothing :
    value
    ->
        Traversal
            (Maybe value)
            value
            (Maybe valueMapped)
            valueMapped
            { value : valueFocusNamed }
            valueFocusView
            valueFocus
            valueFocusMapped
            valueFocusNamed
            valueFocusView
            valueFocusFocusNamed
valueElseOnNothing fallback =
    traversal
        { description = { structure = "Maybe", focus = "Nothing" }
        , view =
            \valueMap ->
                \maybe ->
                    maybe
                        |> Maybe.withDefault fallback
                        |> valueMap
        , map = Maybe.map
        , focusName =
            \focusFocusNamed -> { value = focusFocusNamed }
        }



-- Result


{-| This accessor lets you view values inside the Ok variant of a Result.

    import Accessors exposing (view, map, onOk)
    import Record

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord =
        { foo = Ok { bar = 2 }
        , qux = Err "Not an Int"
        }

    maybeRecord |> view (Record.foo << onOk << Record.bar)
    --> Just 2

    maybeRecord |> view (Record.qux << onOk << Record.bar)
    --> Nothing

    maybeRecord |> mapOver (Record.foo << onOk << Record.bar) ((+) 1)
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    maybeRecord |> mapOver (Record.qux << onOk << Record.bar) ((+) 1)
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
onOk :
    Prism
        (Result error value)
        value
        (Result error valueMapped)
        valueMapped
        { ok : valueFocusView }
        valueFocusNamed
        (Result errorFocusNamed valueFocusNamed)
        valueFocusView
        valueFocusView
        valueFocusFocusNamed
onOk =
    prism
        { description = { structure = "Result", focus = "Ok" }
        , view = Result.toMaybe
        , map = Result.map
        , focusName = \value -> { ok = value }
        }


{-| This accessor lets you view values inside the Err variant of a Result.

    import Accessors exposing (view, map, onErr)
    import Record

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord =
        { foo = Ok { bar = 2 }
        , qux = Err "Not an Int"
        }

    maybeRecord |> view (Record.foo << onErr)
    --> Nothing

    maybeRecord |> view (Record.qux << onErr)
    --> Just "Not an Int"

    maybeRecord |> mapOver (Record.foo << onErr) String.toUpper
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    maybeRecord |> mapOver (Record.qux << onErr) String.toUpper
    --> { foo = Ok { bar = 2 }, qux = Err "NOT AN INT" }

-}
onErr :
    Prism
        (Result error value)
        error
        (Result errorMapped value)
        errorMapped
        { err : errorFocusView }
        errorFocusNamed
        (Result errorFocusNamed valueFocusNamed)
        errorFocusView
        errorFocusView
        errorFocusFocusNamed
onErr =
    prism
        { description = { structure = "Result", focus = "Err" }
        , view =
            \result ->
                case result of
                    Ok _ ->
                        Nothing

                    Err error ->
                        error |> Just
        , map = Result.mapError
        , focusName = \error -> { err = error }
        }
