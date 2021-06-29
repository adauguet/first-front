module UI.Color exposing (black, hexToColor, primary, white)

import Element exposing (Color, rgb255)


primary : Color
primary =
    rgb255 255 100 100


white : Color
white =
    rgb255 255 255 255


black : Color
black =
    rgb255 0 0 0



-- helpers


hexToColor : Int -> Color
hexToColor =
    hexToRgb >> (\( r, g, b ) -> rgb255 r g b)


hexToRgb : Int -> ( Int, Int, Int )
hexToRgb hex =
    let
        ( r, remainder ) =
            divModBy (256 * 256) hex

        ( g, b ) =
            divModBy 256 remainder
    in
    ( r, g, b )


divModBy : Int -> Int -> ( Int, Int )
divModBy by n =
    ( n // by, modBy by n )
