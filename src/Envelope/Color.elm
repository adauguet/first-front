module Envelope.Color exposing (Color(..), equals, toColor, toString)

import Element exposing (rgb255)


type Color
    = Ivory
    | White
    | Cream
    | Flecked
    | BlueKingFisher
    | Fuchsia


toString : Color -> String
toString color =
    case color of
        Ivory ->
            "Ivoire"

        White ->
            "Blanche"

        Cream ->
            "Crème"

        Flecked ->
            "Mouchetée"

        BlueKingFisher ->
            "Bleue Martin-pêcheur"

        Fuchsia ->
            "Fuschia"


equals : Color -> Color -> Bool
equals color1 color2 =
    case ( color1, color2 ) of
        ( Ivory, Ivory ) ->
            True

        ( White, White ) ->
            True

        ( Cream, Cream ) ->
            True

        ( Flecked, Flecked ) ->
            True

        ( BlueKingFisher, BlueKingFisher ) ->
            True

        ( Fuchsia, Fuchsia ) ->
            True

        _ ->
            False


toColor : Color -> Element.Color
toColor color =
    case color of
        Ivory ->
            rgb255 255 255 240

        White ->
            rgb255 255 255 255

        Cream ->
            rgb255 255 253 208

        Flecked ->
            rgb255 199 155 114

        BlueKingFisher ->
            rgb255 45 113 182

        Fuchsia ->
            rgb255 212 66 105
