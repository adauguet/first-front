module Page.Pricing exposing (Model, Msg, init, update, view)

import Element
    exposing
        ( Attribute
        , Element
        , alignRight
        , centerX
        , column
        , el
        , fill
        , height
        , none
        , padding
        , paddingEach
        , paragraph
        , px
        , row
        , spacing
        , text
        , textColumn
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Option)
import Envelope exposing (Envelope)
import Envelope.Color exposing (Color(..))
import Envelope.Format exposing (Format(..))
import Envelope.Pricing
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), frenchLocale)
import List.Extra as List
import UI
import UI.Color.Tailwind as Color


type alias Model =
    { envelopes : List Envelope
    , format : Maybe Format
    , selected : Maybe Envelope
    , quantity : String
    }


init : Model
init =
    let
        envelopes =
            Envelope.references
    in
    { envelopes = envelopes
    , selected = List.head envelopes
    , format = List.head envelopes |> Maybe.map .format
    , quantity = "1"
    }


type Msg
    = DidSelectFormat Format
    | DidSelectEnvelope Envelope
    | DidInputQuantity String


update : Msg -> Model -> Model
update msg model =
    case msg of
        DidSelectFormat format ->
            { model
                | format = Just format
                , selected =
                    model.envelopes
                        |> List.filter (\envelope -> Envelope.Format.equals format envelope.format)
                        |> List.head
            }

        DidSelectEnvelope envelope ->
            { model | selected = Just envelope }

        DidInputQuantity quantity ->
            { model | quantity = quantity }


view : Model -> Element Msg
view model =
    column [ padding 32, spacing 32, width fill ]
        [ el [ Font.size 32 ] <| text "Tarifs"
        , case model.selected of
            Nothing ->
                none

            Just envelope ->
                column [ spacing 32, centerX ]
                    [ Input.radio [ spacing 8 ]
                        { onChange = DidSelectFormat
                        , options =
                            model.envelopes
                                |> List.map .format
                                |> List.uniqueBy Envelope.Format.toString
                                |> List.map (\format -> Input.option format (text <| Envelope.Format.toString format))
                        , selected = model.format
                        , label = Input.labelAbove [ paddingBottom 12 ] <| text "Format"
                        }
                    , case model.format of
                        Just format_ ->
                            Input.radio [ spacing 8 ]
                                { onChange = DidSelectEnvelope
                                , options =
                                    model.envelopes
                                        |> List.filter (\e -> Envelope.Format.equals e.format format_)
                                        |> List.map colorOption
                                , selected = model.selected
                                , label = Input.labelAbove [ paddingBottom 12 ] <| text "Couleur"
                                }

                        Nothing ->
                            none
                    , Input.text [ Font.alignRight ]
                        { onChange = DidInputQuantity
                        , text = model.quantity
                        , placeholder = Nothing
                        , label = Input.labelAbove [ paddingBottom 12 ] <| text "Quantité"
                        }
                    , case String.toInt model.quantity of
                        Just int ->
                            let
                                subTotal =
                                    Envelope.Pricing.total int envelope.pricing

                                shipping =
                                    4.99

                                format float =
                                    FormatNumber.format { frenchLocale | decimals = Exact 2, positiveSuffix = " €" } float
                            in
                            column [ width fill, spacing 16 ]
                                [ row [ width fill, spacing 8 ]
                                    [ text "Sous-total"
                                    , el [ alignRight ] <| text <| format subTotal
                                    ]
                                , row [ width fill, spacing 8 ]
                                    [ text "Frais de port"
                                    , el [ alignRight ] <| text <| format shipping
                                    ]
                                , row [ width fill, spacing 8, Font.bold ]
                                    [ text "Total"
                                    , el [ alignRight ] <| text <| format (shipping + subTotal)
                                    ]
                                ]

                        Nothing ->
                            none
                    , textColumn [ width fill, spacing 16 ]
                        [ paragraph [] [ text "Pour passer commande, appelez-nous 😉" ]
                        , el [] <| UI.callLink UI.callLinkAttributes
                        ]
                    ]
        ]


paddingBottom : Int -> Attribute msg
paddingBottom int =
    paddingEach { top = 0, left = 0, right = 0, bottom = int }


colorOption : Envelope -> Option Envelope Msg
colorOption envelope =
    Input.option envelope <|
        row [ spacing 12 ]
            [ el
                [ width <| px 44
                , height <| px 44
                , Background.color <| Envelope.Color.toColor envelope.color
                , Border.rounded 4
                , Border.width 1
                , Border.color Color.warmGray300
                ]
                none
            , text <| Envelope.Color.toString envelope.color
            ]



-- unPair : (a -> b ->c) -> ( a, b ) -> c
-- unPair f ( a, b ) =
--     f a b
