module UI exposing
    ( callLink
    , callLinkAttributes
    , callLinkWhiteAttributes
    , gray
    , logo
    , logoWhite
    , logoWithoutEmoji
    , secondary
    , white
    )

import Element
    exposing
        ( Attribute
        , Color
        , Element
        , el
        , link
        , paddingXY
        , rgb255
        , text
        )
import Element.Background as Background
import Element.Border as Border
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


callLink : List (Attribute msg) -> Element msg
callLink attributes =
    link
        attributes
        { url = "tel:+33638377163"
        , label = text "06 38 37 71 63"
        }


callLinkAttributes : List (Attribute msg)
callLinkAttributes =
    [ Background.color secondary
    , paddingXY 32 16
    , Border.rounded 5
    , Font.color white
    , Font.semiBold
    , Border.glow gray 2
    ]


callLinkWhiteAttributes : List (Attribute msg)
callLinkWhiteAttributes =
    [ Background.color white
    , paddingXY 32 16
    , Border.rounded 5
    , Font.color secondary
    , Font.semiBold
    ]
