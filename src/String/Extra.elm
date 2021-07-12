module String.Extra exposing (removeAllCaps)

import String exposing (split)


removeAllCaps : String -> String
removeAllCaps string =
    string
        |> split " "
        |> List.map toTitleIfUpper
        |> String.join " "


toTitleIfUpper : String -> String
toTitleIfUpper string =
    if String.all Char.isUpper string then
        case String.uncons string of
            Just ( head, tail ) ->
                String.cons head (String.toLower tail)

            Nothing ->
                string

    else
        string
