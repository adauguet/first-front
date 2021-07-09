module Page.Order exposing (Model, Msg, init, update, view)

import Address exposing (Address)
import Csv.Parser as Csv
import Element
    exposing
        ( Attribute
        , Device
        , DeviceClass(..)
        , Element
        , Orientation(..)
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , inFront
        , link
        , minimum
        , moveLeft
        , moveUp
        , none
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , rgb255
        , row
        , shrink
        , spacing
        , spacingXY
        , text
        , textColumn
        , transparent
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Option)
import Envelope exposing (Envelope)
import Envelope.Color exposing (Color(..))
import Envelope.Format exposing (Format(..))
import Envelope.Pricing
import File exposing (File)
import File.Select
import Font as Font_ exposing (Font)
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), frenchLocale)
import List.Extra as List
import Millimeter exposing (Millimeter, cm, mm)
import String.Extra
import Task
import UI
import UI.Color as Color
import UI.Color.Tailwind as Color


type alias Model =
    { envelopes : List Envelope
    , format : Format
    , selectedEnvelope : Envelope
    , fonts : List Font
    , selectedFont : Font
    , addresses : Maybe (List Address)
    , selectedAddressIndex : Int
    , selectedFontColor : Element.Color
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
    , selectedEnvelope = x
    , format = x.format
    , fonts = List.sortBy .name (f :: fs)
    , selectedFont = f
    , addresses = Nothing
    , selectedAddressIndex = 0
    , selectedFontColor = rgb255 1 63 116
    }


type Msg
    = DidSelectFormat Format
    | DidSelectEnvelope Envelope
    | DidSelectFont Font
    | ClickedImport
    | GotCsv File
    | GotString String
    | ClickedPrevious
    | ClickedNext
    | DidSelectFontColor Element.Color
    | ClickedResetAddresses


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DidSelectFormat format ->
            let
                mEnvelope =
                    model.envelopes
                        |> List.filter (\envelope -> Envelope.Format.equalsDimensions format envelope.format)
                        |> List.head
            in
            case mEnvelope of
                Just envelope ->
                    ( { model | format = format, selectedEnvelope = envelope }, Cmd.none )

                Nothing ->
                    ( { model | format = format }, Cmd.none )

        DidSelectEnvelope envelope ->
            ( { model | selectedEnvelope = envelope }, Cmd.none )

        DidSelectFont font ->
            ( { model | selectedFont = font }, Cmd.none )

        ClickedImport ->
            ( model, File.Select.file [ "text/csv" ] GotCsv )

        GotCsv file ->
            ( model, Task.perform GotString <| File.toString file )

        GotString string ->
            case Csv.parse { fieldSeparator = ',' } string of
                Ok rows ->
                    ( { model | addresses = Just <| List.filterMap Address.make rows }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ClickedPrevious ->
            ( updateIndex model (\index -> index - 1), Cmd.none )

        ClickedNext ->
            ( updateIndex model ((+) 1), Cmd.none )

        DidSelectFontColor color ->
            ( { model | selectedFontColor = color }, Cmd.none )

        ClickedResetAddresses ->
            ( { model | addresses = Nothing }, Cmd.none )


updateIndex : Model -> (Int -> Int) -> Model
updateIndex model increment =
    case model.addresses of
        Just addresses ->
            { model | selectedAddressIndex = modBy (List.length addresses) (increment model.selectedAddressIndex) }

        Nothing ->
            model


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

                wording =
                    textColumn [ width shrink ] [ paragraph [] [ text "Pour passer commande, contactez-nous\u{00A0}! 😉" ] ]

                linkConfiguration =
                    { url = "mailto:contact@mespetitesenveloppes.com"
                    , label = text "contact@mespetitesenveloppes.com"
                    }
            in
            case ( device.class, device.orientation ) of
                ( Phone, Portrait ) ->
                    column [ width fill, spacing 16 ]
                        [ wording
                        , link (paddingXY 12 12 :: linkAttributes) linkConfiguration
                        , UI.callLink (paddingXY 12 12 :: linkAttributes)
                        ]

                _ ->
                    column [ spacing 16 ]
                        [ el [ centerX ] <| wording
                        , row [ spacing 16 ]
                            [ link (paddingXY 32 16 :: linkAttributes) linkConfiguration
                            , UI.callLink (paddingXY 32 16 :: linkAttributes)
                            ]
                        ]

        title =
            el [ Font.size 32 ] <| text "Commande"

        addressesView =
            column [ spacing 12 ]
                [ el [ Font.bold ] <| text "Adresses"
                , let
                    loadAddressButton =
                        Input.button
                            [ Background.color Color.primary500
                            , Border.rounded 4
                            , Font.color Color.white
                            , paddingXY 16 8
                            , width fill
                            , Font.center
                            ]
                            { onPress = Just ClickedImport
                            , label = text "Charger mes adresses"
                            }

                    resetAddressesButton =
                        Input.button [ Font.color Color.primary500 ]
                            { onPress = Just ClickedResetAddresses
                            , label = text "Supprimer"
                            }
                  in
                  case model.addresses of
                    Nothing ->
                        loadAddressButton

                    Just [] ->
                        column [ width fill, spacing 8 ]
                            [ text "Aucune adresse chargée"
                            , loadAddressButton
                            ]

                    Just [ _ ] ->
                        column [ width fill, spacing 8 ]
                            [ text "1 adresse chargée"
                            , resetAddressesButton
                            ]

                    Just list ->
                        column [ width fill, spacing 8 ]
                            [ text <| String.fromInt (List.length list) ++ " adresses chargées"
                            , resetAddressesButton
                            ]
                , paragraph [ Font.size 12, Font.color Color.warmGray400 ]
                    [ row [ spacing 4 ]
                        [ UI.faIcon [] "fas fa-info-circle"
                        , text """Pensez à prévoir au minimum 5 mm de marge entre les dimensions du contenu et celles de l'enveloppe.
                            Exemple\u{00A0}: pour un faire-part de de 140\u{00A0}x\u{00A0}140\u{00A0}mm, je choisis une enveloppe de 150\u{00A0}x\u{00A0}150\u{00A0}mm."""
                        ]
                    ]
                ]
    in
    case ( device.class, device.orientation ) of
        ( Phone, _ ) ->
            column [ padding 16, spacing 32, width fill ]
                [ title
                , addressesView
                , column [ alignTop, spacing 32 ]
                    [ envelopeFormatSelect model
                    , envelopeColorSelect model
                    , fontSelect model.fonts model.selectedFont
                    , fontColorSelect model
                    ]
                , preview model 8 (screenWidth - 32)
                , quantityAndPrices model
                , callUs
                ]

        ( Tablet, _ ) ->
            column [ padding 32, spacing 32, width fill ]
                [ title
                , column [ width fill, spacing 64 ]
                    [ row [ width fill, spacing 32 ]
                        [ column [ alignTop, width (px 250), spacing 32 ]
                            [ addressesView
                            , envelopeFormatSelect model
                            , envelopeColorSelect model
                            , fontSelect model.fonts model.selectedFont
                            , fontColorSelect model
                            ]
                        , el [ alignTop ] <| preview model 10 400
                        ]
                    , column [ centerX, spacing 32 ]
                        [ quantityAndPrices model
                        , callUs
                        ]
                    ]
                ]

        _ ->
            column [ width shrink, centerX, paddingXY 64 32, spacing 32 ]
                [ title
                , column [ width shrink, spacing 64 ]
                    [ row [ width shrink, spacing 32 ]
                        [ column
                            [ alignTop
                            , alignLeft
                            , spacing 32
                            , height fill
                            , width (px 250)
                            ]
                            [ addressesView
                            , envelopeFormatSelect model
                            , envelopeColorSelect model
                            ]
                        , el
                            [ centerX
                            , alignTop
                            , height fill
                            , width fill
                            ]
                            (preview model 10 500)
                        , column
                            [ alignTop
                            , height fill
                            , alignRight
                            , spacing 32
                            ]
                            [ fontSelect model.fonts model.selectedFont
                            , fontColorSelect model
                            ]
                        ]
                    , column [ centerX, spacing 32 ]
                        [ el [ centerX ] <| quantityAndPrices model
                        , callUs
                        ]
                    ]
                ]


envelopeFormatSelect : Model -> Element Msg
envelopeFormatSelect model =
    column [ spacing 16 ]
        [ Input.radio [ spacing 8 ]
            { onChange = DidSelectFormat
            , options =
                model.envelopes
                    |> List.map .format
                    |> List.uniqueBy Envelope.Format.toString
                    |> List.map (\format -> Input.option format (text <| Envelope.Format.toString format))
            , selected = Just model.format
            , label = Input.labelAbove [ paddingBottom 12, Font.bold ] <| text "Format de l'enveloppe"
            }
        , paragraph [ Font.size 12, Font.color Color.warmGray400 ]
            [ row [ spacing 4 ]
                [ UI.faIcon [] "fas fa-info-circle"
                , text """Pensez à prévoir au minimum 5 mm de marge entre les dimensions du contenu et celles de l'enveloppe.
                            Exemple\u{00A0}: pour un faire-part de de 140\u{00A0}x\u{00A0}140\u{00A0}mm, je choisis une enveloppe de 150\u{00A0}x\u{00A0}150\u{00A0}mm."""
                ]
            ]
        ]


envelopeColorSelect : Model -> Element Msg
envelopeColorSelect model =
    Input.radio [ spacing 8 ]
        { onChange = DidSelectEnvelope
        , options =
            model.envelopes
                |> List.filter (\e -> Envelope.Format.equalsDimensions e.format model.format)
                |> List.map envelopeColorOption
        , selected = Just model.selectedEnvelope
        , label = Input.labelAbove [ paddingBottom 12, Font.bold ] <| text "Couleur"
        }


fontColorSelect : Model -> Element Msg
fontColorSelect model =
    let
        colorButton color =
            Input.button []
                { onPress = Just <| DidSelectFontColor color
                , label =
                    if Color.equals model.selectedFontColor color then
                        el
                            [ width <| px 24
                            , height <| px 24
                            , Background.color Color.white
                            , Border.rounded 4
                            , Border.width 1
                            , Border.color Color.warmGray300
                            ]
                        <|
                            el
                                [ width <| px 18
                                , height <| px 18
                                , Background.color color
                                , Border.rounded 3
                                , centerX
                                , centerY
                                ]
                                none

                    else
                        el
                            [ width <| px 24
                            , height <| px 24
                            , Background.color color
                            , Border.rounded 4
                            , Border.width 1
                            , Border.color Color.warmGray300
                            ]
                            none
                }
    in
    Color.rosemood
        |> List.map colorButton
        |> wrappedRow [ spacingXY 5 5, width <| px 200 ]


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


envelopeColorOption : Envelope -> Option Envelope Msg
envelopeColorOption envelope =
    let
        size =
            24
    in
    Input.option envelope <|
        row [ spacing 12 ]
            [ el
                [ width <| px size
                , height <| px size
                , Background.color <| Envelope.Color.toColor envelope.color
                , Border.rounded 4
                , Border.width 1
                , Border.color Color.warmGray300
                ]
                none
            , text <| Envelope.Color.toString envelope.color
            ]


pxPerMm : Format -> Int -> Float
pxPerMm format widthPx =
    toFloat widthPx
        / (case format of
            Square sizeMM ->
                Millimeter.toFloat sizeMM

            Rectangle _ widthMM ->
                Millimeter.toFloat widthMM
          )


preview : Model -> Int -> Int -> Element Msg
preview model scaleFontSize envelopeWidth =
    let
        selectedAddress =
            case model.addresses of
                Nothing ->
                    Address.placeholder

                Just [] ->
                    Address.placeholder

                Just [ address ] ->
                    address

                Just addresses ->
                    List.getAt model.selectedAddressIndex addresses
                        |> Maybe.withDefault Address.placeholder

        pxToMm px =
            0.26458333333719 * toFloat px

        mmToPx : Millimeter -> Int
        mmToPx millimeter =
            round <| Millimeter.toFloat millimeter * pxPerMm model.selectedEnvelope.format envelopeWidth
    in
    column
        [ width <| px envelopeWidth
        , spacing 16
        ]
        [ column
            [ width fill
            , spacing 4
            ]
            [ column [ alignRight, width <| px <| mmToPx (mm 10) ]
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
            , let
                envelopeHeight =
                    case model.selectedEnvelope.format of
                        Square _ ->
                            envelopeWidth

                        Rectangle h w ->
                            round <| toFloat envelopeWidth * Millimeter.toFloat h / Millimeter.toFloat w

                fontSize =
                    round <| pxToMm model.selectedFont.previewSize * pxPerMm model.selectedEnvelope.format envelopeWidth

                format font string =
                    if font.isAllCapsCompatible then
                        string

                    else
                        String.Extra.removeAllCaps string
              in
              el
                [ alignTop
                , width <| px envelopeWidth
                , height <| px <| envelopeHeight
                , Background.color <| Envelope.Color.toColor model.selectedEnvelope.color
                , Border.rounded 4
                , Border.width 1
                , Border.color Color.warmGray300
                ]
              <|
                textColumn
                    [ alignRight
                    , alignBottom
                    , moveUp <| toFloat <| mmToPx <| cm 3
                    , moveLeft <| toFloat <| mmToPx <| cm 2
                    , spacing 0
                    , Font.family [ Font_.typeface model.selectedFont ]
                    , width <| minimum (round <| toFloat envelopeWidth / 2.5) <| shrink
                    , Font.color model.selectedFontColor
                    , Font.size fontSize
                    , padding 8
                    ]
                    [ paragraph [] [ text <| format model.selectedFont selectedAddress.name ]
                    , paragraph [] [ text <| format model.selectedFont selectedAddress.street ]
                    , paragraphIfNotEmpty <| format model.selectedFont selectedAddress.streetLine2
                    , paragraph []
                        [ row [ spacing 8 ]
                            [ textIfNotEmpty <| format model.selectedFont selectedAddress.postalCode
                            , textIfNotEmpty <| format model.selectedFont selectedAddress.city
                            ]
                        ]
                    , paragraphIfNotEmpty <| format model.selectedFont selectedAddress.country
                    ]
            ]
        , case model.addresses of
            Nothing ->
                none

            Just [] ->
                none

            Just [ _ ] ->
                el [ centerX, Font.color Color.warmGray400 ] <| text "1 / 1"

            Just addresses ->
                let
                    index =
                        model.selectedAddressIndex + 1

                    total =
                        List.length addresses
                in
                row [ width fill, spacing 8, Font.size 12 ]
                    [ Input.button [ Font.color Color.primary500 ] { onPress = Just ClickedPrevious, label = text "précédent" }
                    , el [ centerX, Font.color Color.warmGray400 ] <| text <| String.fromInt index ++ " / " ++ String.fromInt total
                    , Input.button [ Font.color Color.primary500, alignRight ] { onPress = Just ClickedNext, label = text "suivant" }
                    ]
        , paragraph
            [ Font.color Color.warmGray400
            , Font.size 12
            , transparent model.selectedFont.isAllCapsCompatible
            , width fill
            ]
            [ row [ spacing 4 ]
                [ UI.faIcon [] "fas fa-info-circle"
                , text "Pour cette police, les mots en lettres majuscules sont convertis en lettres minuscules par souci de lisibilité."
                ]
            ]
        ]


textIfNotEmpty : String -> Element msg
textIfNotEmpty string =
    if String.isEmpty string then
        none

    else
        text string


paragraphIfNotEmpty : String -> Element msg
paragraphIfNotEmpty string =
    if String.isEmpty string then
        none

    else
        paragraph [] [ text string ]


quantityAndPrices : Model -> Element Msg
quantityAndPrices model =
    let
        format : (Int -> Float) -> String
        format compute =
            case quantity of
                0 ->
                    "- €"

                amount ->
                    formatAmount <| compute amount

        formatAmount amount =
            FormatNumber.format { frenchLocale | decimals = Exact 2, positiveSuffix = " €" } amount

        shipping : Float
        shipping =
            4.99

        computeSubTotal =
            Envelope.Pricing.total model.selectedEnvelope.pricing

        quantity : Int
        quantity =
            case model.addresses of
                Just addresses ->
                    List.length addresses

                Nothing ->
                    0

        rowSpacing : Int
        rowSpacing =
            32
    in
    column [ spacing 16 ]
        [ row [ width fill, spacing rowSpacing ]
            [ text "Quantité"
            , el [ alignRight, width <| px 100, Font.alignRight ] <| text <| String.fromInt quantity
            ]
        , row [ width fill, spacing rowSpacing ]
            [ text "Prix unitaire"
            , el [ alignRight, width <| px 100, Font.alignRight ] <| text <| formatAmount <| Envelope.Pricing.unitPrice model.selectedEnvelope.pricing 1
            ]
        , row [ width fill, spacing rowSpacing ]
            [ text "Sous-total"
            , el [ alignRight, width <| px 100, Font.alignRight ] <| text <| format computeSubTotal
            ]
        , row [ width fill, spacing rowSpacing ]
            [ text "Frais de port"
            , el [ alignRight, width <| px 100, Font.alignRight ] <| text <| format (\_ -> shipping)
            ]
        , row [ width fill, spacing rowSpacing, Font.bold ]
            [ text "Total"
            , el [ alignRight, width <| px 100, Font.alignRight ] <| text <| format (\q -> computeSubTotal q + shipping)
            ]
        ]
