module Main exposing (..)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key)
import Element
    exposing
        ( Device
        , DeviceClass(..)
        , Element
        , Orientation(..)
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , layout
        , mouseOver
        , none
        , padding
        , paddingXY
        , paragraph
        , row
        , shrink
        , spacing
        , text
        , textColumn
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Page.Home as Home
import Page.Pricing as Pricing
import Route exposing (Route)
import UI
import UI.Color as Color
import UI.Color.Tailwind as Color
import Url exposing (Url)


type alias Model =
    { key : Key
    , device : Device
    , page : Page
    }


type Page
    = Home
    | Pricing
    | NotFound


fromRoute : Maybe Route -> Page
fromRoute mRoute =
    case mRoute of
        Just Route.Home ->
            Home

        Just Route.Pricing ->
            Pricing

        Nothing ->
            NotFound


init : Flags -> Url -> Key -> ( Model, Cmd msg )
init { window } url key =
    ( { key = key
      , device = Element.classifyDevice window
      , page = fromRoute <| Route.parse url
      }
    , Cmd.none
    )


type Msg
    = UrlChanged Url
    | ClickedLink UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    case msg of
        UrlChanged url ->
            ( { model | page = fromRoute <| Route.parse url }, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case Route.parse url of
                        Just route ->
                            ( model, Route.push model.key route )

                        Nothing ->
                            ( model, Cmd.none )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )


view : Model -> Document msg
view model =
    { title = "Mes Petites Enveloppes"
    , body = [ body model ]
    }


body : Model -> Html msg
body model =
    layout [ Font.family [ Font.typeface "Poppins" ] ] <|
        column
            [ width fill
            , height fill
            ]
            [ header model.device
            , content model
            ]


content : Model -> Element msg
content { device, page } =
    case page of
        Home ->
            Home.view device

        Pricing ->
            Pricing.view

        NotFound ->
            textColumn
                [ width shrink, centerX, centerY, spacing 32, padding 32 ]
                [ paragraph
                    [ Font.center
                    , Font.size 64
                    ]
                    [ text "😢" ]
                , paragraph
                    [ Font.center
                    , Font.size 32
                    , Font.family [ Font.typeface "Caveat" ]
                    ]
                    [ text "La page que vous avez demandée n'existe pas !" ]
                ]


header : Device -> Element msg
header { class, orientation } =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            el
                [ width fill
                , padding 16
                , Background.color Color.primary
                , Region.navigation
                ]
                (UI.logoWhite 28)

        _ ->
            row
                [ width fill
                , padding 32
                , Background.color Color.primary
                , Region.navigation
                , spacing 64
                ]
                [ UI.logoWhite 48
                , Route.link
                    [ alignRight
                    , Font.color Color.white
                    , paddingXY 16 8
                    , mouseOver [ Font.color Color.warmGray200 ]
                    ]
                    { route = Route.Pricing, label = text "Tarifs" }
                , UI.callLink (alignRight :: UI.callLinkWhiteAttributes)
                ]


type alias Flags =
    { window :
        { height : Int
        , width : Int
        }
    }


main : Program Flags Model Msg
main =
    application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = ClickedLink
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
