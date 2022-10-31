Describe how to reach a structure's content
to [map](map#over)/[`view`](map#view) arbitrary content more easily

## reach 

- 1:1, e.g.
    - a record's `.score` field value
    - a tuple's second value
- 1:n, e.g.
    - a `List`'s contained 0-n elements
    - a `Set Int`'s contained 0-n even elements
- 1:?, e.g.
    - a `Maybe`'s 0-1 values
    - a `List`'s 0-1 head elements

```elm
import Map

recordFoo : Map.PartMappingToSameType { record | foo : foo } foo fooView
recordFoo =
    Map.part "foo"
        { access = .foo
        , map = \alter record -> { record | foo = record.foo |> alter }
        }

recordBar : Map.PartMappingToSameType { record | bar : bar } bar barView
recordBar =
    Map.part "bar"
        { access = .bar
        , map = \alter record -> { record | bar = record.bar |> alter }
        }

onJust :
    Map.Possibility
        (Maybe value)
        value
        valueView
        (Maybe valueMapped)
        valueMapped
onJust =
    Map.Possibility "Just"
        { view = identity
        , map = Maybe.Map
        }

each :
    Map.Elements
        (List element)
        element
        (List elementView)
        elementView
        (List elementMapped)
        elementMapped
each = 
    Map.elements "element each"
        { view = List.Map
        , map = List.Map
        }
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
    |> Map.view (Record.foo << List.Map.each << Record.bar)
--> [ 3, 2, 0 ]

fooBars
    |> Map.over
        (recordFoo << List.Map.each << Record.bar)
        (\n -> n * 2)
--> { foo = [ { bar = 6 }, { bar = 4 }, { bar = 0 } ] }
```

## type-safe, reusable

Reaching into on non-matching data structures will yield nice
compile-time errors:

```elm
fooBars |> Map.view (Record.foo << Record.foo)
```
> The 2nd argument to `view` is not what I expect:
> 
>     ..| view (recordFoo << recordFoo) myData
>                                       ^^^^^^
> This `myData` value is a:
> 
>     { foo : List { bar : number } }
> 
> But `view` needs the 2nd argument to be:
> 
>     { foo : { a | foo : c } }

Any reach you create can be composed with any other to match your new
data structures: 

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
