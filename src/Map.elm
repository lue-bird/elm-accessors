module Map exposing
    ( Map, Alter
    , ToMapped(..)
    , at
    , description
    , over, overLazy
    , onJust
    , onOk, onErr
    )

{-| Map content inside a nested structure

@docs Map, Alter
@docs ToMapped


## create

@docs at


## scan

@docs description


## nested map

@docs over, overLazy


## for `Maybe`

@docs onJust


## for `Result`

@docs onOk, onErr

-}


{-| Change elements. Examples

  - each `Array` element
  - e.g. the value of one of many variants
      - [`onJust`](#onJust)
      - [`onOk`](#onOk)
      - [`onErr`](#onErr)
  - [`Tuple.Map.first`](Tuple-Map#first)
  - [`SelectList.Map.selected`](SelectList-Map#selected)
  - record `.field` value

Intuitively, its type could look like

    type alias Map structure part mapped partMapped =
        { map : (part -> partMapped) -> (structure -> mapped)
        , description : List String
        }

Unfortunately, we then need `Map.toPart`, `Map.toTranslate`, `Map.toMaybe`, ... compose functions.

Defining "Lens" in terms of [`ToMapped`](#ToMapped)s

    ToMapped part partMapped -> ToMapped structure mapped

we're able to make use of `<<`
to [`Map.over`](#over) a nested structure.

Technical note: This is an approximation of [CPS based / Van Laarhoven encoded lenses](https://www.tweag.io/blog/2022-05-05-existential-optics/)

-}
type alias Map structure element mapped elementMapped =
    ToMapped element elementMapped
    -> ToMapped structure mapped


{-| Description on how to `map` a value

[`Map`](#Map) always expect a [`ToMapped`](#ToMapped) and build a new [`ToMapped`](#ToMapped) with it

-}
type ToMapped value mapped
    = ToMapped
        { toMapped : value -> mapped
        , description : List String
        }



-- mapping to same type


{-| [`Map`](#Map) that will only preserve the element type.

`Alter` can be used to simplify argument and result types

    module List.Map exposing (onHead)

    import Map

    onHead : Alter (List element) element
    onHead =
        Map.at "0"
            (\alter list ->
                case list of
                    [] ->
                        []

                    head :: tail ->
                        (head |> alter) :: tail
            )

-}
type alias Alter structure element =
    Map structure element structure element



--


{-| The `<<` chain gives us a `List` of unique [`description`](#description)s.
This is useful when you want type-safe identifiers for a `Dict`
similar to the way you'd use a Sum type's constructors to key a dictionary for a form
but you still want to use the `elm/core` implementation.

    import Map
    import Dict.Map
    import Record

    (Record.email
        << Map.onJust
        << Record.info
        << Dict.Map.valueAtString "subject"
    )
        |> Map.description
        |> String.join ")"
    --> "email)Just)info)subject"

-}
description :
    Map structure element mapped elementMapped
    -> List String
description =
    \map ->
        let
            (ToMapped toMappedInternal) =
                map
                    (ToMapped
                        { toMapped =
                            -- as this will never be called, we can do any
                            -- shenanigans we want to make `description` take a `Map` that can also map
                            \_ ->
                                let
                                    runForeverButProduceANewTypeVariableInTheory : () -> newTypeVariable
                                    runForeverButProduceANewTypeVariableInTheory () =
                                        runForeverButProduceANewTypeVariableInTheory ()
                                in
                                runForeverButProduceANewTypeVariableInTheory ()
                        , description = []
                        }
                    )
        in
        toMappedInternal.description


{-| Create a [`Map`](#Map) from

  - a `String` that uniquely describes the part
  - a function that changes elements inside the structure

```
foo : Alter { record | foo : foo } foo
foo =
    Map.at "foo"
        (\alter record -> { record | foo = record.foo |> alter })

onOk : Map (Result error value) value (Result error valueMapped) valueMapped
onOk =
    Map.at "Ok" Result.Map

each : Map (List element) element (List elementMapped) elementMapped
each =
    Map.at "each" List.Map
```

-}
at :
    String
    ->
        ((element -> elementMapped)
         -> (structure -> mapped)
        )
    -> Map structure element mapped elementMapped
at focusDescription map =
    \(ToMapped elementToMapped) ->
        ToMapped
            { description =
                focusDescription :: elementToMapped.description
            , toMapped = map elementToMapped.toMapped
            }


{-| Transform elements
as shown in a given [`Map`](#Map) with a given function

    import Record

    { foo = { qux = 0 } }
        |> Map.over (Record.foo << Record.qux) (\n -> n + 1)
    --> { foo = { qux = 1 } }

-}
over :
    Map structure element mapped elementMapped
    ->
        ((element -> elementMapped)
         -> (structure -> mapped)
        )
over map change =
    let
        (ToMapped toMapped) =
            map
                (ToMapped
                    { description = []
                    , toMapped = change
                    }
                )
    in
    toMapped.toMapped


{-| [`Map.over`](#over) which checks that the old and the new version are different
before giving you the changed/original structure back.

Useful when used together with `Html.lazy`, because it uses reference
equality for complex structures. Therefore, using lazy `map` will
not prevent `Html.lazy` from doing its work.

-}
overLazy :
    Alter structure element
    ->
        ((element -> element)
         -> (structure -> structure)
        )
overLazy alter change =
    \structure ->
        let
            changedStructure =
                structure |> over alter change
        in
        if changedStructure /= structure then
            changedStructure

        else
            structure



-- Maybe


{-| map the value inside `Maybe`

    import Map exposing (onJust)
    import Record

    maybeRecord : { foo : Maybe { bar : Int }, qux : Maybe { bar : Int } }
    maybeRecord =
        { foo = Just { bar = 2 }
        , qux = Nothing
        }

    maybeRecord
        |> Map.over (Record.foo << onJust << Record.bar) (\n -> n + 1)
    --> { foo = Just { bar = 3 }, qux = Nothing }

    maybeRecord
        |> Map.over (Record.qux << onJust << Record.bar) (\n -> n + 1)
    --> { foo = Just { bar = 2 }, qux = Nothing }

-}
onJust : Map (Maybe value) value (Maybe valueMapped) valueMapped
onJust =
    at "Just" Maybe.map



-- Result


{-| map the value inside the `Ok` variant of a `Result`

    import Map exposing (onOk)
    import Record

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord =
        { foo = Ok { bar = 2 }
        , qux = Err "Not an Int"
        }

    maybeRecord
        |> Map.over
            (Record.foo << onOk << Record.bar)
            (\n -> n + 1)
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    maybeRecord
        |> Map.over
            (Record.qux << onOk << Record.bar)
            (\n -> n + 1)
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
onOk :
    Map
        (Result error value)
        value
        (Result error valueMapped)
        valueMapped
onOk =
    at "Ok" Result.map


{-| map the value inside the `Err` variant of a `Result`

    import Map exposing (onErr)
    import Record

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord =
        { foo = Ok { bar = 2 }
        , qux = Err "Not an Int"
        }

    maybeRecord
        |> Map.over (Record.foo << onErr) String.toUpper
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    maybeRecord
        |> Map.over (Record.qux << onErr) String.toUpper
    --> { foo = Ok { bar = 2 }, qux = Err "NOT AN INT" }

-}
onErr :
    Map
        (Result error value)
        error
        (Result errorMapped value)
        errorMapped
onErr =
    at "Err" Result.mapError
