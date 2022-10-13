module Reach exposing
    ( Part, Maybe, Elements, ViewMap(..)
    , ElementsMappingToSameType, MaybeMappingToSameType, PartMappingToSameType
    , part, maybe, elements
    , flat
    , view, has
    , description
    , mapOver, mapOverLazy
    , onJust, onNothing
    , onOk, onErr
    )

{-| Reach into nested structures to [map](#mapOver) or [`view`](#view) arbitrary content

@docs Part, Maybe, Elements, ViewMap


## mapping to same type

@docs ElementsMappingToSameType, MaybeMappingToSameType, PartMappingToSameType


## create

@docs part, maybe, elements


## alter

@docs flat


## scan

@docs view, has
@docs description


## nested map

@docs mapOver, mapOverLazy


## for `Maybe`

@docs onJust, onNothing


## for `Result`

@docs onOk, onErr

-}


{-| Reach an inner value that is always present: 1:1. Examples

  - [`Tuple.Reach.first`](Tuple-Reach#first)
  - [`SelectList.Reach.selected`](SelectList-Reach#selected)
  - record `.field` value

Intuitively, its type could look like

    type alias Lens structure part mapped partMapped =
        { access : structure -> part
        , map : (part -> partMapped) -> (structure -> mapped)
        }

Unfortunately, we then need `Reach.toPart`, `Reach.toTranslate`, `Reach.toMaybe`, ... compose functions.

Defining "Lens" in terms of [`ViewMap`](#ViewMap)s:

    ViewMap part partView partMapped
    -> ViewMap structure partView mapped

we're able to make use of `<<`
to [`view`](#view)/[map](#mapOver) a nested structure.

Technical note: This is an approximation of [CPS based / Van Laarhoven encoded lenses](https://www.tweag.io/blog/2022-05-05-existential-optics/)

-}
type alias Part structure part partView mapped partMapped =
    Elements structure part partView partView mapped partMapped


{-| Reach a value that might or might not exist: 1:Maybe. Examples

  - [`onJust`](#onJust)
  - [`onOk`](#onOk)
  - [`onErr`](#onErr)

haskell/... devs will recognize this as a prism

-}
type alias Maybe structure possibility possibilityView mapped possibilityMapped =
    Elements
        structure
        possibility
        (Maybe.Maybe possibilityView)
        possibilityView
        mapped
        possibilityMapped


{-| Reach multiple elements: 1:n. Examples

  - e.g. each `Array` element

↓ are defined using [`Reach.Elements`](Reach#Elements) of some kind

  - [`Reach.Part`](Reach#Part): 1:1
      - e.g. a `SelectList`'s selected element
  - [`Reach.Maybe`](Reach#Maybe): 1:Maybe
      - e.g. the value of one of many variants

haskell/... devs will recognize this as a traversal

-}
type alias Elements structure element view elementView mapped elementMapped =
    ViewMap element elementView elementMapped
    -> ViewMap structure view mapped


{-| Description on how to `view` or `map` a value

[`Reach.Elements`](Reach#Elements) and its descendants
always expect a [`ViewMap`](#ViewMap) and build a new [`ViewMap`](#ViewMap) with it

-}
type ViewMap value view mapped
    = ViewMap
        { view : value -> view
        , map : value -> mapped
        , description : List String
        }



-- mapping to same type


{-| [`Reach.Part`](Reach#Part) that can only alter the inner part, not change its type.

`PartMappingToSameType` can be used to simplify argument or result types:

    module Record exposing (score)

    import Reach

    score : Reach.PartMappingToSameType { record | score : score } score scoreView
    score =
        Reach.part "score"
            { access = .score
            , map = \alter record -> { record | score = record.score |> alter }
            }

-}
type alias PartMappingToSameType structure part partView =
    Part structure part partView structure part


{-| [`Reach.Maybe`](Reach#Maybe) that can only alter the possibility, not change its type.

`MaybeMappingToSameType` can be used to simplify argument or result types:

    module List.Reach exposing (onHead)

    import Reach

    onHead : Reach.MaybeMappingToSameType (List element) element elementView
    onHead =
        Reach.maybe "0"
            { access = List.head
            , map =
                \alter list ->
                    case list of
                        [] ->
                            []

                        head :: tail ->
                            (head |> alter) :: tail
            }

-}
type alias MaybeMappingToSameType structure possibility possibilityView =
    Maybe structure possibility possibilityView structure possibility


{-| [`Reach.Elements`](Reach#Elements) that can only alter the elements, not change their type.

`ManyMappingToSameType` can be used to simplify argument or result types.

-}
type alias ElementsMappingToSameType structure possibility view possibilityView =
    Elements structure possibility view possibilityView structure possibility



--


{-| The parts reached

    import Record
    import List.Reach

    { foo = { bar = "filling" } }
        |> Reach.view (Record.foo << Record.bar)
    --> "filling"

    { foo = [ { bar = "filling" } ] }
        |> Reach.view (Record.foo << List.Reach.elementEach << Record.bar)
    --> [ "filling" ]

-}
view :
    Elements structure reach view reach mapped reachMapped
    -> (structure -> view)
view reachMany =
    reachMany |> viewAndDescription |> .view


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type-safe.

    import List.Reach

    Just 1234
        |> Reach.has onJust
    --> True

    Nothing
        |> Reach.has onJust
    --> False

    [ "Stuff", "things" ]
        |> Reach.has (List.Reach.element 2)
    --> False

    [ "Stuff", "things" ]
        |> Reach.has (List.Reach.element 0)
    --> True

-}
has :
    Maybe structure reach reach mapped reachMapped
    -> (structure -> Bool)
has reachMaybe =
    \structure ->
        (structure |> view reachMaybe) /= Nothing


{-| Each reach has a name.
The `<<` chain gives us a `List` of unique reach [`description`](#description)s.
This is useful when you want type-safe identifiers for a `Dict`
similar to the way you'd use a Sum type's constructors to key a dictionary for a form
but you still want to use the `elm/core` implementation.

    import Reach
    import Dict.Reach
    import Record

    (Record.email
        << onJust
        << Record.info
        << Dict.Reach.valueAtString "subject"
    )
        |> Reach.description
        |> String.join ")"
    --> "email)Just)info)value at subject"

-}
description :
    Elements structure reach view reach mapped reach
    -> List String
description =
    \reachMany ->
        reachMany |> viewAndDescription |> .description


viewAndDescription :
    Elements structure reach view reach mapped reachMapped
    ->
        { description : List String
        , view : structure -> view
        }
viewAndDescription reach =
    let
        (ViewMap viewMap) =
            reach
                (ViewMap
                    { description = []
                    , view = identity
                    , map =
                        -- as this will never be called, we can do any
                        -- shenanigans we want to make `viewAndDescription` take a `Reach.Elements` that can also map
                        \_ ->
                            let
                                runForeverButProduceANewTypeVariableInTheory : () -> newTypeVariable
                                runForeverButProduceANewTypeVariableInTheory () =
                                    runForeverButProduceANewTypeVariableInTheory ()
                            in
                            runForeverButProduceANewTypeVariableInTheory ()
                    }
                )
    in
    { description = viewMap.description
    , view = viewMap.view
    }


{-| Create a 1:1 [`Reach.Part`](Reach#Part) from

  - a `String` that uniquely describes the part
  - a function that [accesses](#view) the structure's part
  - a function that changes the part inside the structure

```
foo : Reach.Part { record | foo : foo } foo fooView { record | foo : foo } foo
foo =
    Reach.part "foo"
        { access = .foo
        , map = \alter record -> { record | foo = record.foo |> alter }
        }
```

-}
part :
    String
    ->
        { access : structure -> reach
        , map : (reach -> reachMapped) -> (structure -> mapped)
        }
    -> Part structure reach reachView mapped reachMapped
part focusDescription reach =
    elements focusDescription
        { view =
            \reachView structure ->
                structure |> reach.access |> reachView
        , map = reach.map
        }


{-| Create a 1:Maybe [`Reach.Maybe`](Reach#Maybe) from

  - a `String` that uniquely describes the part
  - a function that _tries_ to [access](#view) the structure's part
  - a function that changes the part inside the structure

```
onOk : Reach.Maybe (Result error value) value focusFocus focusFocusView
onOk =
    Reach.maybe "Ok"
        { access = Result.toMaybe
        , map = Result.map
        }
```

-}
maybe :
    String
    ->
        { access : structure -> Maybe.Maybe possibility
        , map : (possibility -> possibilityMapped) -> (structure -> mapped)
        }
    -> Maybe structure possibility reachView mapped possibilityMapped
maybe focusDescription reach =
    elements focusDescription
        { view =
            \focusFocusView structure ->
                structure |> reach.access |> Maybe.map focusFocusView
        , map = reach.map
        }


{-| Create a 1:n [`Reach.Elements`](Reach#Elements) from

  - a `String` that uniquely describes the n elements
  - a function that applies a given [access](#view) function to the structure's n elements
  - a function that changes the n elements inside the structure

```
elementEach :
    Traversal
        (List element)
        element
        (List elementFocusView)
        elementFocus
        elementFocusView
elementEach =
    traversal "element each"
        { view = List.map
        , map = List.map
        }
```

-}
elements :
    String
    ->
        { view : (reach -> reachView) -> (structure -> view)
        , map : (reach -> reachMapped) -> (structure -> mapped)
        }
    -> Elements structure reach view reachView mapped reachMapped
elements focusDescription reach =
    \(ViewMap deeper) ->
        ViewMap
            { view = reach.view deeper.view
            , map = reach.map deeper.map
            , description =
                deeper.description
                    |> (::) focusDescription
            }


{-| Only relevant if you want to [`view`](#view) a structure.

Chaining [`Reach.Maybe`](#Maybe)s can have an undesired consequence
for the result of [`view`](#view):

    import List.Reach
    import Record

    Just { info = Just [ "we", "are", "Maybe something?" ] }
        |> Reach.view
            (onJust
                << Record.info
                << onJust
                << List.Reach.element 2
            )
        --> Just (Just (Just "Maybe something?"))

Nested `Just`s is rarely what you want.
Instead, [`Reach.flat`](#flat)
before every [`Reach.Maybe`](#Maybe) except the last one in chain
so only one `Maybe` is left to [`view`](#view)

    import List.Reach
    import Record

    Just { info = Just [ "we", "are", "Maybe something?" ] }
        |> Reach.view
            (Reach.flat
                << onJust
                << Record.info
                << Reach.flat
                << onJust
                << List.Reach.element 2
            )
        --> Just "Maybe something?"

It's like calling `Maybe.andThen identity` on what we get from there on

-}
flat :
    Elements
        structure
        structure
        (Maybe.Maybe valueView)
        (Maybe.Maybe (Maybe.Maybe valueView))
        mapped
        mapped
flat =
    \reach ->
        reach
            |> viewOnly
                (\viewNested -> viewNested |> Maybe.andThen identity)


{-| Don't map, just specify a way to view.

    Reach.flat =
        Reach.viewOnly (Maybe.andThen identity)

    reachValueElseOnNothing fallback =
        Reach.viewOnly (Maybe.withDefault fallback)

-}
viewOnly :
    (view -> viewChanged)
    ->
        Elements
            structure
            structure
            viewChanged
            view
            mapped
            mapped
viewOnly viewChange =
    \(ViewMap deeper) ->
        ViewMap
            { view =
                \structure ->
                    structure
                        |> deeper.view
                        |> viewChange
            , map = deeper.map
            , description = deeper.description
            }


{-| Given a reach and a change for each element
`mapOver` transforms the data `structure`'s reached content.

    import Record

    { foo = { qux = 0 } }
        |> Reach.mapOver (Record.foo << Record.qux) (\n -> n + 1)
    --> { foo = { qux = 1 } }

-}
mapOver :
    Elements structure element view element mapped elementMapped
    ->
        ((element -> elementMapped)
         -> (structure -> mapped)
        )
mapOver reach change =
    let
        (ViewMap structureViewMap) =
            reach
                (ViewMap
                    { description = []
                    , view = identity
                    , map = change
                    }
                )
    in
    structureViewMap.map


{-| Lazy version of [`mapOver`](#mapOver).

These actions check that the old and the new version are different before writing.
They are useful when used together with `Html.lazy`, because it uses reference
equality for complex structures. Therefore, using lazy `map` will
not prevent `Html.lazy` from doing its work.

-}
mapOverLazy :
    Elements structure reach view reach structure reach
    ->
        ((reach -> reach)
         -> (structure -> structure)
        )
mapOverLazy reach change =
    \structure ->
        let
            changedStructure =
                structure |> mapOver reach change
        in
        if
            (changedStructure |> view reach)
                /= (structure |> view reach)
        then
            changedStructure

        else
            structure



-- Maybe


{-| Reach the value inside `Maybe`

    import Reach exposing (onJust)
    import Record

    maybeRecord : { foo : Maybe.Maybe { bar : Int }, qux : Maybe.Maybe { bar : Int } }
    maybeRecord =
        { foo = Just { bar = 2 }
        , qux = Nothing
        }

    maybeRecord
        |> Reach.view (Record.foo << onJust << Record.bar)
    --> Just 2

    maybeRecord
        |> Reach.view (Record.qux << onJust << Record.bar)
    --> Nothing

    maybeRecord
        |> Reach.mapOver (Record.foo << onJust << Record.bar) (\n -> n + 1)
    --> { foo = Just { bar = 3 }, qux = Nothing }

    maybeRecord
        |> Reach.mapOver (Record.qux << onJust << Record.bar) (\n -> n + 1)
    --> { foo = Just { bar = 2 }, qux = Nothing }

To view nested [`Reach.Maybe`](#Maybe)s flattened, [`Reach.flat`](#flat)

-}
onJust :
    Maybe
        (Maybe.Maybe value)
        value
        valueView
        (Maybe.Maybe valueMapped)
        valueMapped
onJust =
    maybe "Just"
        { access = identity
        , map = Maybe.map
        }


{-| Reach the value inside `Maybe`

    import Reach exposing (onNothing)
    import Record

    maybeRecord : { foo : Maybe.Maybe { bar : Int }, qux : Maybe.Maybe { bar : Int } }
    maybeRecord =
        { foo = Just { bar = 2 }
        , qux = Nothing
        }

    maybeRecord
        |> Reach.has (Record.foo << onNothing)
    --> False

    maybeRecord
        |> Reach.has (Record.qux << onNothing)
    --> True

There's no real use case for reaching into variants with 0 values except for [`Reach.has`](Reach#has)

-}
onNothing :
    Maybe
        (Maybe.Maybe value)
        ()
        unitView
        (Maybe.Maybe value)
        ()
onNothing =
    maybe "Nothing"
        { access =
            \maybeStructure ->
                case maybeStructure of
                    Nothing ->
                        () |> Just

                    Just _ ->
                        Nothing
        , map = \_ -> identity
        }



-- Result


{-| Reach the value inside the `Ok` variant of a `Result`

    import Reach exposing (onOk)
    import Record

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord =
        { foo = Ok { bar = 2 }
        , qux = Err "Not an Int"
        }

    maybeRecord |> Reach.view (Record.foo << onOk << Record.bar)
    --> Just 2

    maybeRecord |> Reach.view (Record.qux << onOk << Record.bar)
    --> Nothing

    maybeRecord
        |> Reach.mapOver
            (Record.foo << onOk << Record.bar)
            (\n -> n + 1)
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    maybeRecord
        |> Reach.mapOver
            (Record.qux << onOk << Record.bar)
            (\n -> n + 1)
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
onOk :
    Maybe
        (Result error value)
        value
        valueView
        (Result error valueMapped)
        valueMapped
onOk =
    maybe "Ok"
        { access = Result.toMaybe
        , map = Result.map
        }


{-| Reach the value inside the `Err` variant of a `Result`

    import Reach exposing (onErr)
    import Record

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord =
        { foo = Ok { bar = 2 }
        , qux = Err "Not an Int"
        }

    maybeRecord |> Reach.view (Record.foo << onErr)
    --> Nothing

    maybeRecord |> Reach.view (Record.qux << onErr)
    --> Just "Not an Int"

    maybeRecord
        |> Reach.mapOver (Record.foo << onErr) String.toUpper
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    maybeRecord
        |> Reach.mapOver (Record.qux << onErr) String.toUpper
    --> { foo = Ok { bar = 2 }, qux = Err "NOT AN INT" }

-}
onErr :
    Maybe
        (Result error value)
        error
        errorView
        (Result errorMapped value)
        errorMapped
onErr =
    maybe "Err"
        { access =
            \result ->
                case result of
                    Ok _ ->
                        Nothing

                    Err error ->
                        error |> Just
        , map = Result.mapError
        }
