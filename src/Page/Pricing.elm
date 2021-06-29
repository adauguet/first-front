module Page.Pricing exposing (Model, Msg, init, update, view)

import Element
    exposing
        ( Element
        , column
        , indexedTable
        , none
        , padding
        , paragraph
        , shrink
        , spacing
        , spacingXY
        , table
        , text
        , textColumn
        )
import Element.Font as Font
import Element.Input as Input
import List.Extra as List
import Pricing exposing (Pricing, Step)
import Range


type alias Model =
    List ( String, Envelope )


init : Model
init =
    [ ( ""
      , { format = Square 140
        , color = Ivory
        , pricing =
            ( 0.44
            , [ Step 50 0.38
              , Step 200 0.3
              , Step 1000 0.26
              ]
            )
        }
      )
    ]


type Msg
    = InputTestValue Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputTestValue index value ->
            if String.all Char.isDigit value then
                case List.getAt index model of
                    Just ( _, envelope ) ->
                        List.setAt index ( value, envelope ) model

                    Nothing ->
                        model

            else
                model


view : Model -> Element Msg
view model =
    textColumn [ padding 32, spacing 32 ]
        [ paragraph [ Font.size 32 ] [ text "Tarifs" ]
        , indexedTable []
            { data = model
            , columns =
                [ { header = none
                  , width = shrink
                  , view = enveloppeView
                  }
                ]
            }
        ]


enveloppeView : Int -> ( String, Envelope ) -> Element Msg
enveloppeView index ( quantity, { format, color, pricing } ) =
    column [ spacing 32 ]
        [ text <| formatToString format
        , text <| colorToString color
        , table [ spacingXY 32 8 ]
            { data = Range.fromPricing pricing
            , columns =
                [ { header = none
                  , width = shrink
                  , view =
                        \{ from, to } ->
                            text <|
                                case to of
                                    Just to_ ->
                                        String.fromInt from ++ " - " ++ String.fromInt to_

                                    Nothing ->
                                        String.fromInt from ++ " +"
                  }
                , { header = none
                  , width = shrink
                  , view =
                        \{ price } ->
                            text <| String.fromFloat price ++ " €"
                  }
                ]
            }
        , Input.text []
            { onChange = InputTestValue index
            , text = quantity
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Quantité"
            }
        , case String.toInt quantity of
            Just int ->
                text <| String.fromFloat (Pricing.total int pricing) ++ " €"

            Nothing ->
                none
        ]


type alias Envelope =
    { format : Format
    , color : Color
    , pricing : Pricing
    }


type Format
    = Square Int


formatToString : Format -> String
formatToString format =
    case format of
        Square size ->
            "Carrée " ++ String.fromInt size ++ " x " ++ String.fromInt size


type Color
    = Ivory


colorToString : Color -> String
colorToString color =
    case color of
        Ivory ->
            "Ivoire"
