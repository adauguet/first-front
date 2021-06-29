module Main exposing (..)

import Browser exposing (Document, document)
import Element
    exposing
        ( Device
        , DeviceClass(..)
        , Element
        , Orientation(..)
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
import Element.Region as Region
import Html exposing (Html)
import UI


type alias Model =
    { device : Device }


init : Flags -> Model
init { window } =
    { device = Element.classifyDevice window }


view : Model -> Document msg
view model =
    { title = "💌 Mes Petites Enveloppes"
    , body = [ body model ]
    }


body : Model -> Html msg
body { device } =
    layout [ Font.family [ Font.typeface "Poppins" ] ] <|
        column
            [ width fill
            , height fill
            ]
            [ header device
            , content device
            ]


header : Device -> Element msg
header { class, orientation } =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            el
                [ width fill
                , padding 16
                , Background.color UI.secondary
                , Region.navigation
                ]
            <|
                UI.logoWhite 28

        _ ->
            row
                [ width fill
                , padding 32
                , Background.color UI.secondary
                , Region.navigation
                ]
                [ UI.logoWhite 48
                , el [ alignRight ] <| callLinkWhite
                ]


content : Device -> Element msg
content { class, orientation } =
    let
        importContactParagraph : Element msg
        importContactParagraph =
            textColumn [ width fill, spacing 32, alignTop ]
                [ paragraph
                    [ Font.size 32
                    , Font.color UI.secondary
                    , Font.family [ Font.typeface "Lora" ]
                    , Region.heading 1
                    ]
                    [ text "Importez vos contacts" ]
                , textColumn [ width fill, spacing 16 ]
                    [ paragraph [ spacing 8 ] [ text "Importez directement vos adresses sous forme de tableau Excel." ]
                    , paragraph [ spacing 8 ] [ text "Vous pouvez aussi ajouter une adresse manuellement." ]
                    ]
                ]

        chooseEnvelopesParagraph : Element msg
        chooseEnvelopesParagraph =
            column [ width fill, spacing 32, alignTop ]
                [ paragraph
                    [ Font.size 32
                    , Font.color UI.secondary
                    , Font.family [ Font.typeface "Lora" ]
                    , Region.heading 1
                    ]
                    [ text "Choisissez vos enveloppes" ]
                , textColumn [ width fill, spacing 16 ]
                    [ paragraph [ spacing 8 ] [ text "Choisissez parmi une large gamme d'enveloppes de haute qualité : format, dimensions, couleur, fermeture ..." ]
                    , paragraph [ spacing 8 ] [ text "Vous avez déjà vos enveloppes ? Envoyez-les nous !" ]
                    ]
                ]

        chooseFontParagraph : Element msg
        chooseFontParagraph =
            column [ width fill, spacing 32, alignTop ]
                [ paragraph
                    [ Font.size 32
                    , Font.color UI.secondary
                    , Font.family [ Font.typeface "Lora" ]
                    , Region.heading 1
                    ]
                    [ text "Choisissez la police" ]
                , textColumn [ width fill, spacing 16 ]
                    [ paragraph [ spacing 8 ] [ text "Le choix d'une police est très personnel." ]
                    , paragraph [ spacing 8 ]
                        [ UI.logoWithoutEmoji 28
                        , text " vous propose un vaste choix de polices, sélectionnées une à une par nos soins. Manuscrite ou en lettres d'imprimerie, vous trouverez votre bonheur !"
                        ]
                    ]
                ]

        receiveEnvelopesParagraph : Element msg
        receiveEnvelopesParagraph =
            column [ width fill, spacing 32, alignTop ]
                [ paragraph
                    [ Font.size 32
                    , Font.color UI.secondary
                    , Font.family [ Font.typeface "Lora" ]
                    , Region.heading 1
                    ]
                    [ text "Recevez vos enveloppes imprimées avec vos adresses" ]
                , textColumn [ width fill, spacing 16 ]
                    [ paragraph [ spacing 8 ] [ text "Vous n'avez plus qu'à mettre sous pli !" ]
                    ]
                ]
    in
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            column
                [ width fill
                , height fill
                , padding 32
                , spacing 64
                , Region.mainContent
                ]
                [ textColumn [ width fill, spacing 16 ]
                    [ paragraph [ Font.size 24, Font.family [ Font.typeface "Lora" ] ] [ text "Vous avez mieux à faire que d'écrire vos adresses à la main !" ]
                    , paragraph [] [ text "Mariage, naissance, baptême, communion ... Gagnez du temps sur les tâches chronophages, concentrez-vous sur l'essentiel." ]
                    , paragraph []
                        [ text "Avec "
                        , UI.logoWithoutEmoji 28
                        , text ", achetez vos enveloppes avec vos adresses imprimées dessus."
                        ]
                    ]
                , importContactParagraph
                , chooseEnvelopesParagraph
                , chooseFontParagraph
                , receiveEnvelopesParagraph
                , column [ spacing 16, centerX ]
                    [ el [ centerX ] <| text "Appelez-nous !"
                    , callLink
                    ]
                ]

        _ ->
            column
                [ width fill
                , height fill
                , padding 64
                , spacing 64
                , Region.mainContent
                ]
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
                    [ importContactParagraph
                    , el
                        [ Background.image "img/excel-apprendre.jpg"
                        , width fill
                        , height <| px 300
                        ]
                        none
                    ]
                , row [ width fill, spacing 64 ]
                    [ chooseEnvelopesParagraph
                    , el
                        [ Background.image "img/joanna-kosinska-uGcDWKN91Fs-unsplash.jpg"
                        , width fill
                        , height <| px 300
                        ]
                        none
                    ]
                , row [ width fill, spacing 64 ]
                    [ chooseFontParagraph
                    , el
                        [ Background.image "img/amador-loureiro-BVyNlchWqzs-unsplash.jpg"
                        , width fill
                        , height <| px 300
                        ]
                        none
                    ]
                , row [ width fill, spacing 64 ]
                    [ receiveEnvelopesParagraph
                    , el
                        [ Background.uncropped "img/10176.jpg"
                        , width fill
                        , height <| px 300
                        ]
                        none
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


type alias Flags =
    { window :
        { height : Int
        , width : Int
        }
    }


main : Program Flags Model msg
main =
    document
        { init = \flags -> ( init flags, Cmd.none )
        , view = view
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
