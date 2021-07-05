module UI.Color exposing
    ( black
    , hexToColor
    , primary100
    , primary150
    , primary200
    , primary250
    , primary300
    , primary350
    , primary400
    , primary450
    , primary50
    , primary500
    , primary550
    , primary600
    , primary650
    , primary700
    , primary750
    , primary800
    , primary850
    , primary900
    , primary950
    , white
    )

import Element exposing (Color, rgb255)


primary50 : Color
primary50 =
    hexToColor 0x00FFF0F0


primary100 : Color
primary100 =
    hexToColor 0x00FFE0E0


primary150 : Color
primary150 =
    hexToColor 0x00FFD1D1


primary200 : Color
primary200 =
    hexToColor 0x00FFC1C1


primary250 : Color
primary250 =
    hexToColor 0x00FFB2B2


primary300 : Color
primary300 =
    hexToColor 0x00FFA2A2


primary350 : Color
primary350 =
    hexToColor 0x00FF9393


primary400 : Color
primary400 =
    hexToColor 0x00FF8383


primary450 : Color
primary450 =
    hexToColor 0x00FF7474


primary500 : Color
primary500 =
    rgb255 255 100 100


primary550 : Color
primary550 =
    hexToColor 0x00E65A5A


primary600 : Color
primary600 =
    hexToColor 0x00CC5050


primary650 : Color
primary650 =
    hexToColor 0x00B34646


primary700 : Color
primary700 =
    hexToColor 0x00993C3C


primary750 : Color
primary750 =
    hexToColor 0x00803232


primary800 : Color
primary800 =
    hexToColor 0x00662828


primary850 : Color
primary850 =
    hexToColor 0x004C1E1E


primary900 : Color
primary900 =
    hexToColor 0x00331414


primary950 : Color
primary950 =
    hexToColor 0x00190A0A


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
