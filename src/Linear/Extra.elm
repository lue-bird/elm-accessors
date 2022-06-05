module Linear.Extra exposing (locationToString)

import Linear exposing (DirectionLinear)


directionToArrowString : DirectionLinear -> String
directionToArrowString =
    \direction ->
        case direction of
            Linear.Up ->
                "↑"

            Linear.Down ->
                "↓"


locationToString : ( DirectionLinear, Int ) -> String
locationToString ( direction, index ) =
    [ direction |> directionToArrowString, index |> String.fromInt ]
        |> String.concat
