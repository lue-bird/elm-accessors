TODO!
- remove arrows for element index

# change log

## 3.0.0

- `module Accessors.Library` remove
    - clutter without benefit
    - no need for backwards compatibility
- `module Accessors.Lazy` remove
    - in favor of `Map.overLazy`
- `module Accessors`
    - name → `Map`
        - plural is just more verbose and
        - `Map` is shorter
        - `Map` is nice as a type prefix
    - tuple accessors move to `Tuple.Map`
    - `List` accessors move to `List.Map`
    - `Array` accessors move to `Array.Map`
    - `Dict` accessors move to `Dict.Map`
    - ```elm
      type Relation structure reach view
          = Relation
                { get : structure -> view
                , over : (reach -> reach) -> (structure -> structure)
                , name : String
                }
      ```
      → `description` `List`, type change on map allowed, `map` simplified
      ```elm
      type ToMapped value mapped
          = ViewMap
              { map : value -> mapped
              , description : List String
              }
      ```
    - `Setable` remove
        - type described every uncomposable traversal
    - `set` remove
        - in favor of `over ... (\_ -> )` which supplies the value to replace with lazily
    - `type alias ..._`s remove
    - `def`, `or`, indexed/keyed versions, non-structure-preserving maps remove
    - `Accessor` name → `Map.Elements`
    - `Lens` name → `Map.Part`
    - `is` name → `has`
    - `try` name → `onJust`
    - `ok` name → `onOk`
    - `err` name → `onErr`
    - `name` → `|> description |> String.join ")"`
    - `makeOneToOne`, `makeOneToN` remove
        - no need for backwards compatibility
    - `makeOneToOne_`, `makeOneToN_` → `Map.at`
    - `get` remove
    - `name : -> String` name → `description : List String`
    - `Alter` add
    - `overLazy` add
