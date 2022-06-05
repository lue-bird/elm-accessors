module Record exposing (age, bar, element, email, foo, info, name, qux, stuff, things, value)

import Accessor exposing (Lens)


makeOneToOne_ :
    String
    -> (structure -> focus)
    -> ((focus -> focus) -> structure -> structure)
    -> Lens structure focus focusFocus focusFocusView
makeOneToOne_ fieldName view alter =
    Accessor.lens
        { description = fieldName
        , view = view
        , map = alter
        }


age : Lens { a | age : b } b focusFocus focusFocusView
age =
    makeOneToOne_ ".age" .age (\alter record -> { record | age = record.age |> alter })


things : Lens { a | things : b } b focusFocus focusFocusView
things =
    makeOneToOne_ ".things" .things (\alter record -> { record | things = record.things |> alter })


stuff : Lens { a | stuff : b } b focusFocus focusFocusView
stuff =
    makeOneToOne_ ".stuff" .stuff (\alter record -> { record | stuff = record.stuff |> alter })


element : Lens { a | element : b } b focusFocus focusFocusView
element =
    makeOneToOne_ ".element" .element (\alter record -> { record | element = record.element |> alter })


value : Lens { a | value : b } b focusFocus focusFocusView
value =
    makeOneToOne_ ".value" .value (\alter record -> { record | value = record.value |> alter })


name : Lens { a | name : b } b focusFocus focusFocusView
name =
    makeOneToOne_ ".name" .name (\alter record -> { record | name = record.name |> alter })


info : Lens { a | info : b } b focusFocus focusFocusView
info =
    makeOneToOne_ ".info" .info (\alter record -> { record | info = record.info |> alter })


email : Lens { a | email : b } b focusFocus focusFocusView
email =
    makeOneToOne_ ".email" .email (\alter record -> { record | email = record.email |> alter })


bar : Lens { a | bar : b } b focusFocus focusFocusView
bar =
    makeOneToOne_ ".bar" .bar (\alter record -> { record | bar = record.bar |> alter })


qux : Lens { a | qux : b } b focusFocus focusFocusView
qux =
    makeOneToOne_ ".qux" .qux (\alter record -> { record | qux = record.qux |> alter })


foo : Lens { a | foo : b } b focusFocus focusFocusView
foo =
    makeOneToOne_ ".foo" .foo (\alter record -> { record | foo = record.foo |> alter })
