module Field exposing (..)

import Accessor exposing (Lens)


makeOneToOne_ :
    String
    -> (structure -> focus)
    -> ((focus -> focus) -> structure -> structure)
    ->
        (Accessor.Relation focus reachable wrap
         -> Accessor.Relation structure reachable wrap
        )
makeOneToOne_ fieldName view alter =
    Accessor.for1To1
        { description = { structure = "record", focus = fieldName }
        , view = view
        , map = alter
        }


age : Lens { record | age : age } transformed age wrap
age =
    makeOneToOne_ ".age" .age (\alter record -> { record | age = record.age |> alter })


things : Lens { record | things : things } transformed things wrap
things =
    makeOneToOne_ ".things" .things (\alter record -> { record | things = record.things |> alter })


stuff : Lens { record | stuff : stuff } transformed stuff wrap
stuff =
    makeOneToOne_ ".stuff" .stuff (\alter record -> { record | stuff = record.stuff |> alter })


name : Lens { record | name : name } transformed name wrap
name =
    makeOneToOne_ ".name" .name (\alter record -> { record | name = record.name |> alter })


info : Lens { record | info : info } transformed info wrap
info =
    makeOneToOne_ ".info" .info (\alter record -> { record | info = record.info |> alter })


email : Lens { record | email : email } transformed email wrap
email =
    makeOneToOne_ ".email" .email (\alter record -> { record | email = record.email |> alter })


bar : Lens { record | bar : bar } transformed bar wrap
bar =
    makeOneToOne_ ".bar" .bar (\alter record -> { record | bar = record.bar |> alter })


qux : Lens { record | qux : qux } transformed qux wrap
qux =
    makeOneToOne_ ".qux" .qux (\alter record -> { record | qux = record.qux |> alter })


foo : Lens { record | foo : foo } transformed foo wrap
foo =
    makeOneToOne_ ".foo" .foo (\alter record -> { record | foo = record.foo |> alter })
