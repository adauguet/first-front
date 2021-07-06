module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Events
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
        , inFront
        , layout
        , mouseOver
        , none
        , padding
        , paddingXY
        , paragraph
        , rgba255
        , row
        , shrink
        , spacing
        , text
        , textColumn
        , width
        )
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Page.Home as Home
import Page.Pricing as Pricing
import Page.Upload as Upload
import Route exposing (Route)
import UI
import UI.Color as Color
import UI.Color.Tailwind as Color
import Url exposing (Url)


type alias Model =
    { key : Key
    , device : Device
    , screenWidth : Int
    , page : Page
    , showMenu : Bool
    }


type Page
    = Home
    | Pricing Pricing.Model
    | Upload Upload.Model
    | NotFound


fromRoute : Maybe Route -> Page
fromRoute mRoute =
    case mRoute of
        Just Route.Home ->
            Home

        Just Route.Pricing ->
            Pricing Pricing.init

        Just Route.Upload ->
            Upload Upload.init

        Nothing ->
            NotFound


init : Window -> Url -> Key -> ( Model, Cmd msg )
init window url key =
    ( { key = key
      , device = Element.classifyDevice window
      , screenWidth = window.width
      , page = fromRoute <| Route.parse url
      , showMenu = False
      }
    , Cmd.none
    )


type Msg
    = UrlChanged Url
    | ClickedLink UrlRequest
    | GotPricingMsg Pricing.Msg
    | GotUploadMsg Upload.Msg
    | ClickedOpenMenu
    | ClickedCloseMenu
    | GotNewWindow Int Int


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

        GotPricingMsg subMsg ->
            case model.page of
                Pricing subModel ->
                    let
                        m =
                            Pricing.update subMsg subModel
                    in
                    ( { model | page = Pricing m }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClickedOpenMenu ->
            ( { model | showMenu = True }, Cmd.none )

        ClickedCloseMenu ->
            ( { model | showMenu = False }, Cmd.none )

        GotNewWindow width height ->
            ( { model
                | device = Element.classifyDevice { width = width, height = height }
                , screenWidth = width
              }
            , Cmd.none
            )

        GotUploadMsg subMsg ->
            case model.page of
                Upload subModel ->
                    let
                        ( m, cmd ) =
                            Upload.update subMsg subModel
                    in
                    ( { model | page = Upload m }, Cmd.map GotUploadMsg cmd )

                _ ->
                    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    let
        fontSize =
            case model.device.class of
                Phone ->
                    14

                _ ->
                    20
    in
    { title = "Mes Petites Enveloppes"
    , body =
        [ layout
            [ Font.family [ Font.typeface "Poppins" ]
            , Font.size fontSize
            , inFront <| modal model.showMenu
            ]
            (body model)
        ]
    }


body : Model -> Element Msg
body model =
    column
        [ width fill
        , height fill
        ]
        [ header model.device
        , content model
        ]


content : Model -> Element Msg
content { device, screenWidth, page } =
    case page of
        Home ->
            Home.view device

        Pricing model ->
            Pricing.view device screenWidth model |> Element.map GotPricingMsg

        Upload model ->
            Upload.view model |> Element.map GotUploadMsg

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


header : Device -> Element Msg
header { class, orientation } =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            column [ width fill ]
                [ row
                    [ width fill
                    , padding 16
                    , Background.color Color.primary500
                    , Region.navigation
                    , spacing 8
                    ]
                    [ Route.link []
                        { route = Route.Home
                        , label = UI.logoWhite 24
                        }
                    , Input.button [ alignRight, Font.color Color.white ]
                        { onPress = Just ClickedOpenMenu
                        , label = UI.faIcon [] "fas fa-bars"
                        }
                    ]
                ]

        ( Phone, Landscape ) ->
            row
                [ width fill
                , padding 16
                , Background.color Color.primary500
                , Region.navigation
                , spacing 8
                ]
                [ Route.link []
                    { route = Route.Home
                    , label = UI.logoWhite 24
                    }
                , Route.link
                    [ alignRight
                    , Font.color Color.white
                    , paddingXY 16 8
                    , mouseOver [ Font.color Color.warmGray200 ]
                    ]
                    { route = Route.Pricing, label = text "Tarifs" }
                ]

        _ ->
            row
                [ width fill
                , padding 32
                , Background.color Color.primary500
                , Region.navigation
                , spacing 64
                ]
                [ Route.link []
                    { route = Route.Home
                    , label = UI.logoWhite 36
                    }
                , Route.link
                    [ alignRight
                    , Font.color Color.white
                    , paddingXY 16 8
                    , mouseOver [ Font.color Color.warmGray200 ]
                    ]
                    { route = Route.Pricing, label = text "Tarifs" }
                , UI.callLink (alignRight :: UI.callLinkWhiteAttributes)
                ]


modal : Bool -> Element Msg
modal showMenu =
    if showMenu then
        menu

    else
        none


menu : Element Msg
menu =
    column
        [ width fill
        , height fill
        , onClick ClickedCloseMenu
        , Background.color <| rgba255 0 0 0 0.2
        ]
        [ column
            [ width fill
            , Background.color Color.white
            ]
            [ row
                [ width fill
                , padding 16
                , Region.navigation
                , spacing 8
                ]
                [ Route.link []
                    { route = Route.Home
                    , label = el [ Font.color Color.primary500 ] <| UI.logo 24
                    }
                , Input.button [ alignRight, Font.color Color.primary500 ]
                    { onPress = Just ClickedCloseMenu
                    , label = UI.faIcon [] "fas fa-times"
                    }
                ]
            , Route.link
                [ Font.color Color.primary500
                , paddingXY 32 16
                , mouseOver [ Font.color Color.warmGray200 ]
                ]
                { route = Route.Pricing, label = text "Tarifs" }
            ]
        ]


type alias Window =
    { height : Int
    , width : Int
    }


main : Program Window Model Msg
main =
    application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = ClickedLink
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize GotNewWindow
