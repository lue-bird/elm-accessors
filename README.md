The Accessors library
=====================

This library provides a way to describe relations between a container and its
content, and use that description to manipulate arbitrary data more easily.

# Build your relations 

There are two kinds of relations between a container and its content: 1:1
relations (e.g. a record and its field) and 1:n relations (e.g. a `List` can
contain 0-n elements, a `Maybe` can contain 0-1 elements).

For 1:1 relations, the `for1To1` function will let you build an accessor
by describing how to view the sub-element from the super-element, and how to map
a function over it. For instance, with a record:

```elm
recordFoo =
    for1To1
        ".foo"
        .foo
        (\alter record -> { record | foo = record.foo |> alter })

recordBar =
    for1To1
        ".bar"
        .bar
        (\alter record -> { record | bar = record.bar |> alter })
```

1:n relations are more complex in terms of abstraction, but they are usually
very easy to implement:

```elm
elementEach = 
    for1ToN
        "List element each"
        List.map
        List.map

onJust =
    for1ToN
        "Maybe.Just"
        Maybe.map
        Maybe.map
```

## combine your relations

Accessors can be composed easily to describe relations:

```elm
fooBars =
    { foo =
        [ { bar = 3 }
        , { bar = 2 }
        , { bar = 0 }
        ]
    }

myAccessor = recordFoo << onEach << recordBar
```

## alter your data easily

Then you use an action function to determine which kind of operation you want to
do on your data using the accessor

```elm
fooBars |> view myAccessor
--> [ 3, 2, 0 ]

fooBars |> map myAccessor (\n -> n * 2)
--> { foo = [ { bar = 6 }, { bar = 4 }, { bar = 0 } ] }
```

# type-safe, reusable

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
bar =
    { bar = Just [ 1, 3, 2 ] }

halfWay =
    try << onEach

myOtherAccessor =
    recordBar << halfWay

bar |> view myOtherAccessor
--> Just [ 1, 3, 2 ]
```

## play with it

[ellie with accessors](https://ellie-app.com/4wHNCxgft87a1). 

## contribute

run

`npx elm-verify-examples` and `elm-test` or `elm-test-rs`

If you write new accessor combinators that rely on common library data, I'll be
happy to review and merge. Please include tests for your combinators.
