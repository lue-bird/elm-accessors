Describe relations between a structure and its
content, and use that description to `map`/`view` arbitrary content more easily.

## build relations 

- 1:1, e.g.
    - a record and a specific field value
    - a tuple and the first or second value
- 1:n, e.g.
    - a `List` can contain 0-n elements
    - a `Maybe` can contain 0-1 elements

```elm
recordFoo =
    Accessor.lens
        { description = { structure = "record", focus = ".foo" }
        , view = .foo
        , map = \alter record -> { record | foo = record.foo |> alter }
        }

recordBar =
    Accessor.lens
        ".bar"
        .bar
        (\alter record -> { record | bar = record.bar |> alter })

elementEach = 
    Accessor.traversal
        { description = { structure = "List", focus = "element each" }
        , view = List.map
        , map = List.map
        }

onJust =
    Accessor.traversal
        { description = { structure = "Maybe", focus = "Just" }
        , view = Maybe.map
        , map = Maybe.map
        }
```

## combine relations

Accessors can be composed easily to describe relations:

```elm
fooBars =
    { foo =
        [ { bar = 3 }
        , { bar = 2 }
        , { bar = 0 }
        ]
    }

fooBars |> view (recordFoo << onEach << recordBar)
--> [ 3, 2, 0 ]

fooBars |> mapOver (recordFoo << onEach << recordBar) (\n -> n * 2)
--> { foo = [ { bar = 6 }, { bar = 4 }, { bar = 0 } ] }
```

## type-safe, reusable

Applying an accessor on non-matching data structures will yield nice
compile-time errors: 

```elm
fooBars |> view (recordFoo << recordFoo)
```
> The 2nd argument to `view` is not what I expect:
> 
> ..| view (recordFoo << recordFoo) myData
>                                     ^^^^^^
> This `myData` value is a:
> 
>     { foo : List { bar : number } }
> 
> But `view` needs the 2nd argument to be:
> 
>     { foo : { a | foo : c } }

Any accessor you make can be composed with any other accessor to match your new
data structures: 

```elm
tryEach =
    onJust << elementEach

myOtherAccessor =
    recordBar << tryEach

{ bar = Just [ 1, 3, 2 ] } |> view myOtherAccessor
--> Just [ 1, 3, 2 ]
```

## play with it

[ellie with accessors](https://ellie-app.com/4wHNCxgft87a1). 

## contribute

run

`npx elm-verify-examples` and `elm-test` or `elm-test-rs`

If you write new accessor combinators that rely on common library data, I'll be
happy to review and merge. Please include tests for your combinators.
