module Route exposing (..)

import Browser.Navigation exposing (Key, pushUrl)
import Element exposing (Attribute, Element)
import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, s, top)


type Route
    = Home
    | Pricing


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Pricing (s "pricing")
        ]


parse : Url -> Maybe Route
parse =
    Url.Parser.parse parser


toPieces : Route -> List String
toPieces route =
    case route of
        Home ->
            []

        Pricing ->
            [ "pricing" ]


toString : Route -> String
toString route =
    "/" ++ String.join "/" (toPieces route)


push : Key -> Route -> Cmd msg
push key route =
    pushUrl key <| toString route


link : List (Attribute msg) -> { route : Route, label : Element msg } -> Element msg
link attributes { route, label } =
    Element.link attributes { url = toString route, label = label }
