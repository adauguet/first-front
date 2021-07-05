module Page.Pricing exposing (Model, Msg, init, update, view)

import Char exposing (isDigit)
import Element
    exposing
        ( Attribute
        , Device
        , DeviceClass(..)
        , Element
        , Orientation(..)
        , alignBottom
        , alignRight
        , alignTop
        , centerX
        , column
        , el
        , fill
        , height
        , inFront
        , link
        , moveLeft
        , moveUp
        , none
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , shrink
        , spacing
        , text
        , textColumn
        , transparent
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
import Font as Font_ exposing (Font)
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), frenchLocale)
import List.Extra as List
import UI
import UI.Color as Color
import UI.Color.Tailwind as Color


type alias Model =
    { envelopes : List Envelope
    , format : Format
    , selected : Envelope
    , quantity : String
    , fonts : List Font
    , font : Font
    }


init : Model
init =
    let
        ( x, xs ) =
            Envelope.references

        ( f, fs ) =
            Font_.all
    in
    { envelopes = x :: xs
    , selected = x
    , format = x.format
    , quantity = "1"
    , fonts = f :: fs
    , font = f
    }


type Msg
    = DidSelectFormat Format
    | DidSelectEnvelope Envelope
    | DidInputQuantity String
    | DidSelectFont Font


update : Msg -> Model -> Model
update msg model =
    case msg of
        DidSelectFormat format ->
            let
                mEnvelope =
                    model.envelopes
                        |> List.filter (\envelope -> Envelope.Format.equals format envelope.format)
                        |> List.head
            in
            case mEnvelope of
                Just envelope ->
                    { model | format = format, selected = envelope }

                Nothing ->
                    { model | format = format }

        DidSelectEnvelope envelope ->
            { model | selected = envelope }

        DidInputQuantity quantity ->
            if String.all isDigit quantity then
                { model | quantity = quantity }

            else
                model

        DidSelectFont font ->
            { model | font = font }


view : Device -> Int -> Model -> Element Msg
view device screenWidth model =
    let
        callUs =
            let
                linkAttributes =
                    [ width fill
                    , Font.center
                    , Font.color Color.white
                    , Background.color Color.primary500
                    , Border.rounded 4
                    ]
            in
            case ( device.class, device.orientation ) of
                ( Phone, Portrait ) ->
                    column [ width fill, spacing 16 ]
                        [ textColumn [ width fill ] [ paragraph [] [ text "Pour passer commande, contactez-nous\u{00A0}! 😉" ] ]
                        , link (paddingXY 12 12 :: linkAttributes)
                            { url = "mailto:contact@mespetitesenveloppes.com"
                            , label = text "contact@mespetitesenveloppes.com"
                            }
                        , UI.callLink (paddingXY 12 12 :: linkAttributes)
                        ]

                _ ->
                    column [ centerX, spacing 16 ]
                        [ textColumn [ width fill ] [ paragraph [] [ text "Pour passer commande, contactez-nous\u{00A0}! 😉" ] ]
                        , link (paddingXY 32 16 :: linkAttributes)
                            { url = "mailto:contact@mespetitesenveloppes.com"
                            , label = text "contact@mespetitesenveloppes.com"
                            }
                        , UI.callLink (paddingXY 32 16 :: linkAttributes)
                        ]
    in
    case ( device.class, device.orientation ) of
        ( Phone, _ ) ->
            column
                [ padding 16
                , spacing 32
                , width fill
                ]
                [ el [ Font.size 32 ] <| text "Tarifs"
                , choices model
                , preview 8 (screenWidth - 32) model.selected model.font
                , quantityAndPrices model
                , callUs
                ]

        _ ->
            column [ padding 32, spacing 32, width fill ]
                [ el [ Font.size 32 ] <| text "Tarifs"
                , column [ centerX, spacing 64 ]
                    [ row [ spacing 64 ]
                        [ choices model
                        , preview 12 500 model.selected model.font
                        , fontSelect model.fonts model.font
                        ]
                    , quantityAndPrices model
                    , callUs
                    ]
                ]


choices : Model -> Element Msg
choices model =
    column [ spacing 32, width fill, alignTop ]
        [ Input.radio [ spacing 8 ]
            { onChange = DidSelectFormat
            , options =
                model.envelopes
                    |> List.map .format
                    |> List.uniqueBy Envelope.Format.toString
                    |> List.map (\format -> Input.option format (text <| Envelope.Format.toString format))
            , selected = Just model.format
            , label = Input.labelAbove [ paddingBottom 12, Font.bold ] <| text "Format"
            }
        , Input.radio [ spacing 8 ]
            { onChange = DidSelectEnvelope
            , options =
                model.envelopes
                    |> List.filter (\e -> Envelope.Format.equals e.format model.format)
                    |> List.map colorOption
            , selected = Just model.selected
            , label = Input.labelAbove [ paddingBottom 12, Font.bold ] <| text "Couleur"
            }
        ]


fontSelect : List Font -> Font -> Element Msg
fontSelect fonts selected =
    Input.radio [ spacing 8 ]
        { onChange = DidSelectFont
        , options =
            fonts
                |> List.map
                    (\font ->
                        Input.option font <|
                            el
                                [ Font.family [ Font_.typeface font ]
                                , Font.size font.size
                                ]
                            <|
                                text font.name
                    )
        , selected = Just selected
        , label = Input.labelAbove [ paddingBottom 12, Font.bold ] <| text "Police"
        }


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


preview : Int -> Int -> Envelope -> Font -> Element msg
preview scaleFontSize size envelope font =
    let
        oneCmToPx =
            case envelope.format of
                Square s ->
                    size * 10 // s

                Rectangle w _ ->
                    size * 10 // w

        fontSize =
            size * font.previewSize // 500
    in
    column
        [ width fill
        , spacing 16
        ]
        [ column [ spacing 16 ]
            [ el
                [ alignTop
                , width <| px size
                , height <| px size
                , Background.color <| Envelope.Color.toColor envelope.color
                , Border.rounded 4
                , Border.width 1
                , Border.color Color.warmGray300
                ]
              <|
                textColumn
                    [ alignRight
                    , alignBottom
                    , moveUp (toFloat size / 3.5)
                    , moveLeft (toFloat size / 6.5)
                    , spacing 4
                    , Font.family [ Font_.typeface font ]
                    , width shrink
                    , Font.color Color.blue800
                    , Font.size fontSize
                    , padding 8
                    ]
                    [ paragraph [] [ text "Monsieur et Madame MACRON" ]
                    , paragraph [] [ text "55 Rue du Faubourg Saint-Honoré" ]
                    , paragraph [] [ text "75008 PARIS" ]
                    ]
            , column [ alignRight, width <| px oneCmToPx ]
                [ el
                    [ width fill
                    , height <| px 4
                    , Border.color Color.warmGray300
                    , Border.widthEach { top = 0, left = 1, right = 1, bottom = 1 }
                    , inFront <| el [ centerX, moveUp 10, Font.size scaleFontSize, Font.color Color.warmGray400 ] <| text "1 cm"
                    ]
                    none
                , el
                    [ width fill
                    , height <| px 3
                    , Border.color Color.warmGray300
                    , Border.widthEach { top = 0, left = 1, right = 1, bottom = 0 }
                    ]
                    none
                ]
            ]
        , textColumn [ width fill ]
            [ paragraph [ Font.color Color.warmGray400, Font.size 12, transparent font.isAllCapsCompatible ]
                [ text "⚠️ Les mots en lettres majuscules sont peu lisibles avec cette police."
                ]
            ]
        ]


quantityAndPrices : Model -> Element Msg
quantityAndPrices model =
    column [ width fill, spacing 8 ]
        [ row [ width fill, spacing 16 ]
            [ el [ Font.bold, width fill ] <| text "Quantité"
            , Input.text [ alignRight, width fill, Font.alignRight ]
                { onChange = DidInputQuantity
                , text = model.quantity
                , placeholder = Nothing
                , label = Input.labelHidden "Quantité"
                }
            ]
        , let
            format : (Int -> Float) -> String
            format compute =
                case String.toInt model.quantity of
                    Nothing ->
                        "- €"

                    Just 0 ->
                        "- €"

                    Just quantity ->
                        FormatNumber.format { frenchLocale | decimals = Exact 2, positiveSuffix = " €" } <| compute quantity

            shipping : Float
            shipping =
                4.99

            computeSubTotal =
                Envelope.Pricing.total model.selected.pricing
          in
          column [ width fill, spacing 16 ]
            [ row [ width fill, spacing 8 ]
                [ text "Sous-total"
                , el [ alignRight ] <| text <| format computeSubTotal
                ]
            , row [ width fill, spacing 8 ]
                [ text "Frais de port"
                , el [ alignRight ] <| text <| format (\_ -> shipping)
                ]
            , row [ width fill, spacing 8, Font.bold ]
                [ text "Total"
                , el [ alignRight ] <| text <| format (\q -> computeSubTotal q + shipping)
                ]
            ]
        ]
