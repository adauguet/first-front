module UI exposing
    ( callLink
    , callLinkAttributes
    , callLinkWhiteAttributes
    , faIcon
    , logo
    , logoWhite
    , logoWithoutEmoji
    )

import Element
    exposing
        ( Attribute
        , Element
        , el
        , html
        , link
        , mouseOver
        , paddingXY
        , text
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (i)
import Html.Attributes exposing (class)
import UI.Color exposing (primary500, white)
import UI.Color.Tailwind as Color


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


callLink : List (Attribute msg) -> Element msg
callLink attributes =
    link
        attributes
        { url = "tel:+33638377163"
        , label = text "06 38 37 71 63"
        }


callLinkAttributes : List (Attribute msg)
callLinkAttributes =
    [ Background.color primary500
    , paddingXY 32 16
    , Border.rounded 5
    , Font.color white
    , Font.semiBold
    ]


callLinkWhiteAttributes : List (Attribute msg)
callLinkWhiteAttributes =
    [ Background.color white
    , paddingXY 32 16
    , Border.rounded 5
    , Font.color primary500
    , Font.semiBold
    , mouseOver
        [ Background.color Color.warmGray100
        ]
    ]


faIcon : List (Attribute msg) -> String -> Element msg
faIcon attributes string =
    el attributes <| html <| i [ class string ] []
