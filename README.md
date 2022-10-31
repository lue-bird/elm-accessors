Describe how to reach a structure's content
to [map](map#over)/[`view`](map#view) arbitrary content more easily

## reach 
Examples

- a part
    - a record's `.score` field value
    - a tuple's second value
- some elements
    - a `List`'s contained 0-n elements
    - a `Set Int`'s contained 0-n even elements
- a possible value
    - a `Maybe`'s 0-1 values
    - a `List`'s 0-1 head elements

```elm
import Map exposing (Map, Alter)

recordFoo : Alter { record | foo : foo } foo
recordFoo =
    Map.at "foo"
        (\alter record -> { record | foo = record.foo |> alter })

recordBar : Alter { record | bar : bar } bar
recordBar =
    Map.at "bar"
        (\alter record -> { record | bar = record.bar |> alter })

onJust : Map (Maybe value) value (Maybe valueMapped) valueMapped
onJust =
    Map.at "Just" Maybe.Map

each : Map (List element) element (List elementMapped) elementMapped
each = 
    Map.elements "each" List.Map
```

## reach deep inside

```elm
fooBars : { foo : List { bar : number } }
fooBars =
    { foo =
        [ { bar = 3 }
        , { bar = 2 }
        , { bar = 0 }
        ]
    }

fooBars
    |> Map.over
        (recordFoo << List.Map.each << recordBar)
        (\n -> n * 2)
--> { foo = [ { bar = 6 }, { bar = 4 }, { bar = 0 } ] }
```

## type-safe, reusable

Reaching into on non-matching data structures will yield nice
compile-time errors

```elm
fooBars |> Map.over (recordFoo << recordFoo) (\n -> n * 2)
```
> The 2nd argument to `over` is not what I expect:
> 
>     ..| over (recordFoo << recordFoo) fooBars
>                                       ^^^^^^
> This `fooBars` value is a:
> 
>     { foo : List { bar : number } }
> 
> But `over` needs the 2nd argument to be:
> 
>     { foo : { a | foo : c } }

Any reach you create can be composed with any other to match your new
data structures

```elm
import Map exposing (onJust)
import List.Map

tryEach =
    onJust << List.Map.each

{ bar = Just [ 1, 3, 2 ] }
    |> Map.over (recordBar << tryEach) negate
--> { bar = Just [ -1, -3, -2 ] }
```

## contribute

run

`elm-review`, `npx elm-verify-examples` and `elm-test`/`elm-test-rs`

If you write new reaches for common library data, I'll be
happy to review and merge. Please include tests.
