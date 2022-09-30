Describe how to reach a structure's content
to [map](Reach#over)/[`view`](Reach#view) arbitrary content more easily

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
import Reach

recordFoo : Reach.PartMappingToSameType { record | foo : foo } foo fooView
recordFoo =
    Reach.part "foo"
        { access = .foo
        , map = \alter record -> { record | foo = record.foo |> alter }
        }

recordBar : Reach.PartMappingToSameType { record | bar : bar } bar barView
recordBar =
    Reach.part "bar"
        { access = .bar
        , map = \alter record -> { record | bar = record.bar |> alter }
        }

onJust :
    Reach.Maybe
        (Maybe value)
        value
        valueView
        (Maybe valueMapped)
        valueMapped
onJust =
    Reach.maybe "Just"
        { view = identity
        , map = Maybe.map
        }

elementEach :
    Reach.Elements
        (List element)
        element
        (List elementView)
        elementView
        (List elementMapped)
        elementMapped
elementEach = 
    Reach.elements "element each"
        { view = List.map
        , map = List.map
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
    |> Reach.view (Record.foo |> Reach.into List.Reach.elementEach |> Reach.into Record.bar)
--> [ 3, 2, 0 ]

fooBars
    |> Reach.mapOver
        (recordFoo |> Reach.into List.Reach.elementEach |> Reach.into Record.bar)
        (\n -> n * 2)
--> { foo = [ { bar = 6 }, { bar = 4 }, { bar = 0 } ] }
```

## type-safe, reusable

Reaching into on non-matching data structures will yield nice
compile-time errors:

```elm
fooBars |> Reach.view (Record.foo |> Reach.into Record.foo)
```
> The 2nd argument to `view` is not what I expect:
> 
>     ..| view (recordFoo |> Reach.into recordFoo) myData
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
import Reach exposing (onJust)
import List.Reach

tryEach =
    onJust |> Reach.into List.Reach.elementEach

{ bar = Just [ 1, 3, 2 ] }
    |> Reach.mapOver (recordBar |> Reach.into tryEach) negate
--> { bar = Just [ -1, -3, -2 ] }
```

## contribute

run

`elm-review`, `npx elm-verify-examples` and `elm-test`/`elm-test-rs`

If you write new reaches for common library data, I'll be
happy to review and merge. Please include tests.
