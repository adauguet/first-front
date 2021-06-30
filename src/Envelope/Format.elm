module Envelope.Format exposing (Format(..), equals, toString)


type Format
    = Square Int
    | Rectangle Int Int


toString : Format -> String
toString format =
    case format of
        Square size ->
            "Carrée " ++ String.fromInt size ++ " x " ++ String.fromInt size ++ " mm"

        Rectangle height width ->
            "Rectangle " ++ String.fromInt height ++ " x " ++ String.fromInt width ++ " mm"


equals : Format -> Format -> Bool
equals format1 format2 =
    case ( format1, format2 ) of
        ( Square size1, Square size2 ) ->
            size1 == size2

        ( Rectangle size11 size12, Rectangle size21 size22 ) ->
            size11 == size21 && size12 == size22

        _ ->
            False
