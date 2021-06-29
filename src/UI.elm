module UI exposing
    ( gray
    , logo
    , logoWhite
    , logoWithoutEmoji
    , secondary
    , white
    )

import Element
    exposing
        ( Color
        , Element
        , el
        , rgb255
        , text
        )
import Element.Font as Font


logo : Int -> Element msg
logo size =
    el
        [ Font.size size
        , Font.family [ Font.typeface "Caveat" ]
        ]
        (text "💌  Mes Petites Enveloppes")


logoWithoutEmoji : Int -> Element msg
logoWithoutEmoji size =
    el
        [ Font.size size
        , Font.family [ Font.typeface "Caveat" ]
        ]
        (text "Mes Petites Enveloppes")


logoWhite : Int -> Element msg
logoWhite size =
    el
        [ Font.size size
        , Font.family [ Font.typeface "Caveat" ]
        , Font.color white
        ]
        (text "💌  Mes Petites Enveloppes")


secondary : Color
secondary =
    rgb255 255 100 100


white : Color
white =
    rgb255 255 255 255


gray : Color
gray =
    rgb255 230 230 230
