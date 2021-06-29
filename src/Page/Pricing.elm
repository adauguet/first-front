module Page.Pricing exposing (view)

import Element
    exposing
        ( Element
        , padding
        , paragraph
        , spacing
        , text
        , textColumn
        )


view : Element msg
view =
    textColumn [ padding 32, spacing 32 ]
        [ paragraph [] [ text "Tarifs" ]
        , paragraph [] [ text "⚠️ Page en construction ⚠️" ]
        ]
