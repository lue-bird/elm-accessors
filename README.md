Describe how to map a structure's inner content easily

```elm
import Map exposing (Map, Alter)

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
    Map.at "Just" Maybe.map

each : Map (List element) element (List elementMapped) elementMapped
each = 
    Map.at "each" List.map
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

Any `Map` you create can be composed with any other to match your new
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

If you write new [`Map`](Map#Map)s for common library data, I'll be
happy to review and merge. Please include tests
