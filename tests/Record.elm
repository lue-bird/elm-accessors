module Record exposing (age, alternative, bar, book, current, element, email, foo, id, info, name, pool, projects, publisher, qux, sales, series, sparkle, stuff, tag, things, trail, value)

import Map exposing (Alter)


makeOneToOne_ :
    String
    -> (structure -> fieldValue)
    -> ((fieldValue -> fieldValue) -> structure -> structure)
    -> Alter structure fieldValue
makeOneToOne_ fieldName _ alter =
    Map.at fieldName alter


book =
    makeOneToOne_ "book" .book (\alter record -> { record | book = record.book |> alter })


series =
    makeOneToOne_ "series" .series (\alter record -> { record | series = record.series |> alter })


publisher =
    makeOneToOne_ "publisher" .publisher (\alter record -> { record | publisher = record.publisher |> alter })


sales =
    makeOneToOne_ "sales" .sales (\alter record -> { record | sales = record.sales |> alter })


trail =
    makeOneToOne_ "trail" .trail (\alter record -> { record | trail = record.trail |> alter })


sparkle =
    makeOneToOne_ "sparkle" .sparkle (\alter record -> { record | sparkle = record.sparkle |> alter })


id =
    makeOneToOne_ "id" .id (\alter record -> { record | id = record.id |> alter })


alternative =
    makeOneToOne_ "alternative" .alternative (\alter record -> { record | alternative = record.alternative |> alter })


current =
    makeOneToOne_ "current" .current (\alter record -> { record | current = record.current |> alter })


tag =
    makeOneToOne_ "tag" .tag (\alter record -> { record | tag = record.tag |> alter })


projects =
    makeOneToOne_ "projects" .projects (\alter record -> { record | projects = record.projects |> alter })


pool =
    makeOneToOne_ "pool" .pool (\alter record -> { record | pool = record.pool |> alter })


age =
    makeOneToOne_ "age" .age (\alter record -> { record | age = record.age |> alter })


things =
    makeOneToOne_ "things" .things (\alter record -> { record | things = record.things |> alter })


stuff =
    makeOneToOne_ "stuff" .stuff (\alter record -> { record | stuff = record.stuff |> alter })


element =
    makeOneToOne_ ".element" .element (\alter record -> { record | element = record.element |> alter })


name =
    makeOneToOne_ "name" .name (\alter record -> { record | name = record.name |> alter })


info =
    makeOneToOne_ "info" .info (\alter record -> { record | info = record.info |> alter })


email =
    makeOneToOne_ "email" .email (\alter record -> { record | email = record.email |> alter })


value =
    makeOneToOne_ "value" .value (\alter record -> { record | value = record.value |> alter })


bar =
    makeOneToOne_ "bar" .bar (\alter record -> { record | bar = record.bar |> alter })


qux =
    makeOneToOne_ "qux" .qux (\alter record -> { record | qux = record.qux |> alter })


foo =
    makeOneToOne_ "foo" .foo (\alter record -> { record | foo = record.foo |> alter })
