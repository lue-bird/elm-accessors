module Lens exposing (..)

import Accessors exposing (Lens, makeOneToOne_)


bar : Lens { record | bar : attribute } transformed attribute built
bar =
    makeOneToOne_ ".bar" .bar (\fn record -> { record | bar = fn record.bar })


foo : Lens { record | foo : attribute } transformed attribute built
foo =
    makeOneToOne_ ".foo" .foo (\fn record -> { record | foo = fn record.foo })


qux : Lens { record | qux : attribute } transformed attribute built
qux =
    makeOneToOne_ ".qux" .qux (\fn record -> { record | qux = fn record.qux })


name : Lens { record | name : attribute } transformed attribute built
name =
    makeOneToOne_ ".name" .name (\fn record -> { record | name = fn record.name })


age : Lens { record | age : attribute } transformed attribute built
age =
    makeOneToOne_ ".age" .age (\fn record -> { record | age = fn record.age })


email : Lens { record | email : attribute } transformed attribute built
email =
    makeOneToOne_ ".email" .email (\fn record -> { record | email = fn record.email })


stuff : Lens { record | stuff : attribute } transformed attribute built
stuff =
    makeOneToOne_ ".stuff" .stuff (\fn record -> { record | stuff = fn record.stuff })


things : Lens { record | things : attribute } transformed attribute built
things =
    makeOneToOne_ ".things" .things (\fn record -> { record | things = fn record.things })


info : Lens { record | info : attribute } transformed attribute built
info =
    makeOneToOne_ ".info" .info (\fn record -> { record | info = fn record.info })
