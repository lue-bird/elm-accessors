TODO!
- remove arrows for element index

# change log

## 3.0.0

- `module Accessors.Library` remove
    - clutter without benefit
    - no need for backwards compatibility
- `module Accessors.Lazy` remove
    - in favor of `Reach.mapOverLazy`
- `module Accessors`
    - name → `Reach`
        - plural is just more verbose and
        - `Reach` is shorter
        - `Reach` is nice as a type prefix
    - tuple accessors move to `Tuple.Reach`
    - `List` accessors move to `List.Reach`
    - `Array` accessors move to `Array.Reach`
    - `Dict` accessors move to `Dict.Reach`
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
      type ViewMap value view mapped
          = ViewMap
              { view : value -> view
              , map : value -> mapped
              , description : List String
              }
      ```
    - `Setable` remove
        - type described _every_ uncomposable traversal
    - `set` remove
        - in favor of `over ... (\_ -> )` which supplies the value to replace with lazily
    - `or` remove
        - in favor of `valueElseOnNothing`
    - `type alias ..._`s remove
    - `def`, `or`, indexed/keyed versions, non-structure-preserving reaches remove
        - in favor of handling directly in the function given to `mapOver` or after `view`
    - `Accessor` name → `Reach.Elements`
    - `Lens` name → `Reach.Part`
    - `is` name → `has`
    - `try` name → `onJust`
    - `ok` name → `onOk`
    - `err` name → `onErr`
    - `def` name → `valueElseOnNothing`
    - `name` → `|> description |> String.join ")"`
    - `makeOneToOne`, `makeOneToN` remove
        - no need for backwards compatibility
    - `makeOneToOne_` name → `Reach.part`
    - `makeOneToN_` name → `Reach.elements`
    - `over` name → `mapOver`
    - `get` name → `view`
    - `name : -> String` name → `description : List String`
    - `Reach.Maybe`, `Reach.maybe` add
    - `Reach....MappingToSameType`s add
    - `mapOverLazy` add
    - `onNothing` add
