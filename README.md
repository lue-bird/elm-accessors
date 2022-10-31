Describe how to map a structure's inner content easily

```elm
import Map exposing (Map, Alter)

effect : { trail : List { sparkle : Int } }
effect =
    { trail =
        [ { sparkle = 3 }
        , { sparkle = 2 }
        , { sparkle = 0 }
        ]
    }

effect
    |> Map.over
        -- no nested update helpers
        (recordTrail << each << recordSparkle)
        (\n -> n * 2)
--> { trail = [ { sparkle = 6 }, { sparkle = 4 }, { sparkle = 0 } ] }

recordTrail : Alter { record | trail : trail } trail
recordTrail =
    Map.at "trail"
        -- exactly how you'd write your helpers
        (\alter record -> { record | trail = record.trail |> alter })

recordSparkle : Alter { record | sparkle : sparkle } sparkle
recordSparkle =
    Map.at "sparkle"
        (\alter record -> { record | sparkle = record.sparkle |> alter })

each : Map (List element) element (List elementMapped) elementMapped
each = 
    Map.at "each" List.map
```

## type-safe, reusable

Reaching into on non-matching data structures will yield nice
compile-time errors

```elm
Map.over (recordTrail << recordTrail) (\n -> n * 2) effect
```
> The 2nd argument to `over` is not what I expect:
> 
>     ..| over (recordTrail << recordTrail) effect
>                                           ^^^^^^
> This `effect` value is a:
> 
>     { trail : List { sparkle : Int } }
> 
> But `over` needs the 2nd argument to be:
> 
>     { trail : { a | trail : c } }

Any `Map` you create can be composed with any other to match your new
data structures

```elm
import Map exposing (onJust)

onJust : Map (Maybe value) value (Maybe valueMapped) valueMapped
onJust =
    Map.at "Just" Maybe.map

tryEach =
    onJust << each

{ sparkle = Just [ 1, 3, 2 ] }
    |> Map.over (recordSparkle << tryEach) negate
--> { sparkle = Just [ -1, -3, -2 ] }
```

## contribute

run

`elm-review`, `npx elm-verify-examples` and `elm-test`/`elm-test-rs`

If you write new [`Map`](Map#Map)s for common library data, I'll be
happy to review and merge. Please include tests
