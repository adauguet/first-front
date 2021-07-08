module Envelope.Format exposing
    ( Format(..)
    , equalsDimensions
    , equalsFormat
    , formatDescription
    , formatImage
    , toString
    )

import Millimeter exposing (Millimeter)


type Format
    = Square Millimeter
    | Rectangle Millimeter Millimeter


toString : Format -> String
toString format =
    case format of
        Square size ->
            "Carrée " ++ Millimeter.toString size ++ " x " ++ Millimeter.toString size ++ " mm"

        Rectangle height width ->
            "Rectangle " ++ Millimeter.toString height ++ " x " ++ Millimeter.toString width ++ " mm"


equalsDimensions : Format -> Format -> Bool
equalsDimensions format1 format2 =
    case ( format1, format2 ) of
        ( Square size1, Square size2 ) ->
            size1 == size2

        ( Rectangle size11 size12, Rectangle size21 size22 ) ->
            size11 == size21 && size12 == size22

        _ ->
            False


equalsFormat : Format -> Format -> Bool
equalsFormat format1 format2 =
    case ( format1, format2 ) of
        ( Square _, Square _ ) ->
            True

        ( Rectangle _ _, Rectangle _ _ ) ->
            True

        _ ->
            False


formatDescription : Format -> String
formatDescription format =
    case format of
        Square _ ->
            "Square"

        Rectangle _ _ ->
            "Rectangle"


formatImage : Format -> String
formatImage format =
    case format of
        Square _ ->
            "img/square.svg"

        Rectangle _ _ ->
            "img/rectangle.svg"
