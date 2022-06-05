# change log

## 3.0.0

- `module Accessors.Library` remove
    - backwards compatibility for just a few users shouldn't clutter the existing API this much
- `module Accessors.Lazy` remove
    - in favor of `Accessors.mapLazy`
- `module Accessors`
    - name → `Accessor`
        - plural is just more verbose and doesn't mirror the type name
    - tuple accessors move to `Tuple.Accessor`
    - `List` accessors move to `List.Accessor`
    - `Array` accessors move to `Array.Accessor`
    - `Dict` accessors move to `Dict.Accessor`
    - `Settable` remove
        - type described _every_ uncomposable traversal
    - `set` remove
        - in favor of `map` which supplies the value to replace with lazily
    - `or` remove
        - in favor of `valueElseOnNothing`
    - `def` name → `valueElseOnNothing`
    - `over` name → `mapOver`
    - `name` → `|> description |> descriptionToString`
    - `Accessors.makeOneToOne_` name → `Accessor.lens`
    - `Accessors.makeOneToN_` name → `Accessor.traversal`
    - `Accessors.makeOneToOne`, `Accessors.makeOneToN` remove
        - backwards compatibility for just a few users shouldn't affect the existing API this much (-`_`)
    - `Optional`, `optional` add
    - `TraversalConsume` add
    - `mapOverLazy` add
