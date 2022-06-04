module Record exposing (..)

import Accessor exposing (Lens)


makeOneToOne_ fieldName view alter =
    Accessor.lens
        { description = { structure = "record", focus = fieldName }
        , view = view
        , map = alter
        , focusName =
            -- should be input
            \focusFocusNamed -> { field = focusFocusNamed }
        }


age =
    makeOneToOne_ ".age" .age (\alter record -> { record | age = record.age |> alter })


things =
    makeOneToOne_ ".things" .things (\alter record -> { record | things = record.things |> alter })


stuff =
    makeOneToOne_ ".stuff" .stuff (\alter record -> { record | stuff = record.stuff |> alter })


name =
    makeOneToOne_ ".name" .name (\alter record -> { record | name = record.name |> alter })


info =
    makeOneToOne_ ".info" .info (\alter record -> { record | info = record.info |> alter })


email =
    makeOneToOne_ ".email" .email (\alter record -> { record | email = record.email |> alter })


bar =
    makeOneToOne_ ".bar" .bar (\alter record -> { record | bar = record.bar |> alter })


qux =
    makeOneToOne_ ".qux" .qux (\alter record -> { record | qux = record.qux |> alter })


foo =
    makeOneToOne_ ".foo" .foo (\alter record -> { record | foo = record.foo |> alter })
