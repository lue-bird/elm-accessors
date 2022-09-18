module Record exposing (age, bar, element, email, foo, info, name, qux, stuff, things, value)

import Reach


makeOneToOne_ :
    String
    -> (structure -> fieldValue)
    -> ((fieldValue -> fieldValue) -> structure -> structure)
    -> Reach.PartMappingToSameType structure fieldValue reachView
makeOneToOne_ fieldName access alter =
    Reach.part fieldName
        { access = access
        , map = alter
        }


age =
    makeOneToOne_ "age" .age (\alter record -> { record | age = record.age |> alter })


things =
    makeOneToOne_ "things" .things (\alter record -> { record | things = record.things |> alter })


stuff =
    makeOneToOne_ "stuff" .stuff (\alter record -> { record | stuff = record.stuff |> alter })


name =
    makeOneToOne_ "name" .name (\alter record -> { record | name = record.name |> alter })


info =
    makeOneToOne_ "info" .info (\alter record -> { record | info = record.info |> alter })


email =
    makeOneToOne_ "email" .email (\alter record -> { record | email = record.email |> alter })


element =
    makeOneToOne_ "element" .element (\alter record -> { record | element = record.element |> alter })


value =
    makeOneToOne_ "value" .value (\alter record -> { record | value = record.value |> alter })


bar =
    makeOneToOne_ "bar" .bar (\alter record -> { record | bar = record.bar |> alter })


qux =
    makeOneToOne_ "qux" .qux (\alter record -> { record | qux = record.qux |> alter })


foo =
    makeOneToOne_ "foo" .foo (\alter record -> { record | foo = record.foo |> alter })
