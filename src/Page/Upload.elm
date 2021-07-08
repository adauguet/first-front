module Page.Upload exposing (Model, Msg, init, update, view)

import Address exposing (Address)
import Csv.Parser as Csv exposing (Problem(..))
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , mouseOver
        , none
        , padding
        , paddingXY
        , row
        , shrink
        , spacing
        , spacingXY
        , table
        , text
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select
import Task
import UI.Color as Color
import UI.Color.Tailwind as Color


type Model
    = Idle
    | Loaded (List Address)
    | Error Csv.Problem


init : Model
init =
    Idle


type Msg
    = ClickedUpload
    | GotFile File
    | GotFileString String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedUpload ->
            ( model, File.Select.file [ "text/csv" ] GotFile )

        GotFile file ->
            ( model, Task.perform GotFileString <| File.toString file )

        GotFileString string ->
            case Csv.parse { fieldSeparator = ',' } string of
                Ok rows ->
                    ( Loaded <| List.filterMap Address.make rows, Cmd.none )

                Err problem ->
                    ( Error problem, Cmd.none )


view : Model -> Element Msg
view model =
    case model of
        Idle ->
            Input.button
                [ centerX
                , centerY
                , Font.color Color.white
                , Background.color Color.primary500
                , paddingXY 32 16
                , Border.rounded 4
                , mouseOver [ Background.color Color.primary550 ]
                ]
                { onPress = Just ClickedUpload
                , label = text "Charger"
                }

        -- Loading ->
        --     el [ centerX, centerY ] <| text "Loading"
        Loaded [] ->
            text "empty"

        Loaded rows ->
            table [ padding 32, spacingXY 16 8 ]
                { data = rows
                , columns =
                    [ { header = none
                      , width = shrink
                      , view = addressView
                      }
                    ]
                }

        Error problem ->
            case problem of
                SourceEndedWithoutClosingQuote _ ->
                    text "Source ended without cloding quote"

                AdditionalCharactersAfterClosingQuote _ ->
                    text "Additional characters after closing quote"


addressView : Address -> Element msg
addressView { name, street, streetLine2, postalCode, city, country } =
    column
        [ spacing 4
        , padding 16
        , Border.glow Color.warmGray200 4
        , Border.rounded 8
        ]
        [ text name
        , text street
        , textIfNotEmpty streetLine2
        , row [ spacing 4 ]
            [ text postalCode
            , text city
            ]
        , textIfNotEmpty country
        ]


textIfNotEmpty : String -> Element msg
textIfNotEmpty string =
    if String.isEmpty string then
        none

    else
        text string
