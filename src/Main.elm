module Main exposing (..)

import Browser exposing (Document, document)
import Element
    exposing
        ( Element
        , alignRight
        , alignTop
        , centerX
        , column
        , el
        , fill
        , height
        , layout
        , link
        , none
        , padding
        , paddingXY
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
import Html exposing (Html)
import UI


type alias Model =
    {}


init : Model
init =
    {}


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "💌 Mes Petites Enveloppes"
    , body = [ body model ]
    }


body : Model -> Html Msg
body _ =
    layout [ Font.family [ Font.typeface "Poppins" ] ] <|
        column
            [ width fill
            , height fill
            ]
            [ headerColor
            , content
            ]


headerColor : Element msg
headerColor =
    row
        [ width fill
        , padding 32
        , Background.color UI.secondary
        ]
        [ UI.logoWhite 48
        , el [ alignRight ] <| callLinkWhite
        ]


content : Element msg
content =
    column
        [ width fill
        , height fill
        , padding 64
        , spacing 16
        ]
        [ column [ width fill, spacing 64 ]
            [ textColumn [ width fill, spacing 16 ]
                [ paragraph [ Font.size 24 ] [ text "Vous avez mieux à faire que d'écrire vos adresses à la main !" ]
                , paragraph [] [ text "Mariage, naissance, baptême, communion ... Gagnez du temps sur les tâches chronophages, concentrez-vous sur l'essentiel." ]
                , paragraph []
                    [ text "Avec "
                    , UI.logoWithoutEmoji 28
                    , text ", achetez vos enveloppes avec vos adresses imprimées dessus."
                    ]
                ]
            , row [ width fill, spacing 64 ]
                [ column [ width fill, spacing 32, alignTop ]
                    [ paragraph [ Font.size 32, Font.color UI.secondary, Font.family [ Font.typeface "Lora" ] ] [ text "1 • Importez vos contacts" ]
                    , textColumn [ spacing 16 ]
                        [ paragraph [ spacing 8 ] [ text "Importez directement votre fichier d'adresses sous forme de tableau Excel." ]
                        , paragraph [ spacing 8 ] [ text "Vous pouvez aussi ajouter une adresse manuellement." ]
                        ]
                    ]
                , el
                    [ Background.image "img/excel-apprendre.jpg"
                    , width fill
                    , height <| px 300
                    ]
                    none
                ]
            , row [ width fill, spacing 64 ]
                [ column [ width fill, spacing 32, alignTop ]
                    [ paragraph [ Font.size 32, Font.color UI.secondary, Font.family [ Font.typeface "Lora" ] ] [ text "2 • Choisissez vos enveloppes" ]
                    , textColumn [ spacing 16 ]
                        [ paragraph [ spacing 8 ] [ text "Choisissez parmi une large gamme d'enveloppes de haute qualité : format, dimensions, couleur, fermeture ..." ]
                        , paragraph [ spacing 8 ] [ text "Vous avez déjà vos enveloppes ? Envoyez-les nous !" ]
                        ]
                    ]
                , el
                    [ Background.image "img/joanna-kosinska-uGcDWKN91Fs-unsplash.jpg"
                    , width fill
                    , height <| px 300
                    ]
                    none
                ]
            , row [ width fill, spacing 64 ]
                [ column [ width fill, spacing 32, alignTop ]
                    [ paragraph [ Font.size 32, Font.color UI.secondary, Font.family [ Font.typeface "Lora" ] ] [ text "3 • Choisissez la police" ]
                    , textColumn [ spacing 16 ]
                        [ paragraph [ spacing 8 ] [ text "Le choix d'une police est très personnel." ]
                        , paragraph [ spacing 8 ]
                            [ UI.logoWithoutEmoji 28
                            , text " vous propose un vaste choix de polices, sélectionnées une à une par nos soins. Manuscrite ou en lettres d'imprimerie, vous trouverez votre bonheur !"
                            ]
                        ]
                    ]
                , el
                    [ Background.image "img/amador-loureiro-BVyNlchWqzs-unsplash.jpg"
                    , width fill
                    , height <| px 300
                    ]
                    none
                ]
            , row [ width fill, spacing 64 ]
                [ column [ width fill, spacing 32, alignTop ]
                    [ paragraph [ Font.size 32, Font.color UI.secondary, Font.family [ Font.typeface "Lora" ] ] [ text "4 • Recevez vos enveloppes imprimées avec vos adresses" ]
                    , textColumn [ spacing 16 ]
                        [ paragraph [ spacing 8 ] [ text "Vous n'avez plus qu'à mettre sous pli !" ]
                        ]
                    ]
                , el
                    [ Background.uncropped "img/10176.jpg"
                    , width fill
                    , height <| px 300
                    ]
                    none
                ]
            ]
        , column [ spacing 16, centerX ]
            [ el [ centerX ] <| text "Appelez-nous !"
            , callLink
            ]
        ]


callLink : Element msg
callLink =
    link
        [ Background.color UI.secondary
        , paddingXY 32 16
        , Border.rounded 5
        , Font.color UI.white
        , Font.semiBold
        , Border.glow UI.gray 2
        ]
        { url = "tel:+33638377163"
        , label = text "06 38 37 71 63"
        }


callLinkWhite : Element msg
callLinkWhite =
    link
        [ Background.color UI.white
        , paddingXY 32 16
        , Border.rounded 5
        , Font.color UI.secondary
        , Font.semiBold
        ]
        { url = "tel:+33638377163"
        , label = text "06 38 37 71 63"
        }


main : Program () Model Msg
main =
    document
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
