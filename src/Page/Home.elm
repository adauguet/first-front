module Page.Home exposing (view)

import Element
    exposing
        ( Device
        , DeviceClass(..)
        , Element
        , Orientation(..)
        , alignTop
        , centerX
        , column
        , el
        , fill
        , height
        , link
        , maximum
        , none
        , padding
        , paragraph
        , px
        , row
        , spacing
        , text
        , textColumn
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region
import UI
import UI.Color as Color


view : Device -> Element msg
view { class, orientation } =
    let
        introParagraph : Int -> Int -> Element msg
        introParagraph size logoSize =
            textColumn [ width fill, spacing 16, Font.size size ]
                [ paragraph [ spacing 8 ] [ text "Vous avez mieux à faire que d'écrire vos adresses à la main !" ]
                , paragraph [ spacing 8 ]
                    [ text "Mariage, naissance, baptême, communion ... "
                    , el [ Font.bold ] <| text "Gagnez du temps"
                    , text " sur les tâches chronophages, concentrez-vous sur l'essentiel."
                    ]
                , paragraph [ spacing 8 ]
                    [ text "Avec "
                    , UI.logoWithoutEmoji logoSize
                    , text ", recevez vos enveloppes avec vos "
                    , el [ Font.bold ] <| text "adresses imprimées"
                    , text " dessus."
                    ]
                ]

        importContactParagraph : Element msg
        importContactParagraph =
            textColumn [ width fill, spacing 32, alignTop ]
                [ paragraph
                    [ Font.size 32
                    , Font.color Color.primary
                    , Font.family [ Font.typeface "Lora" ]
                    , Region.heading 1
                    ]
                    [ text "Importez vos contacts" ]
                , textColumn [ width fill, spacing 16 ]
                    [ paragraph [ spacing 8 ]
                        [ text "Importez directement vos adresses sous forme de tableau Excel."
                        ]
                    , paragraph [ spacing 8 ]
                        [ text "Plusieurs formats sont possibles : "
                        , el [ Font.family [ Font.monospace ] ] <| text ".xls"
                        , text ", "
                        , el [ Font.family [ Font.monospace ] ] <| text ".xlsx"
                        , text ", ou "
                        , el [ Font.family [ Font.monospace ] ] <| text ".csv"
                        , text "."
                        ]
                    , paragraph [ spacing 8 ] [ text "Vous pouvez aussi ajouter une adresse manuellement." ]
                    ]
                ]

        chooseEnvelopesParagraph : Element msg
        chooseEnvelopesParagraph =
            column [ width fill, spacing 32, alignTop ]
                [ paragraph
                    [ Font.size 32
                    , Font.color Color.primary
                    , Font.family [ Font.typeface "Lora" ]
                    , Region.heading 1
                    ]
                    [ text "Choisissez vos enveloppes" ]
                , textColumn [ width fill, spacing 16 ]
                    [ paragraph [ spacing 8 ] [ text "Choisissez parmi une large gamme d'enveloppes de haute qualité : format, dimensions, couleur, fermeture ..." ]
                    , paragraph [ spacing 8, Font.bold ] [ text "Vous avez déjà vos enveloppes ? Envoyez-les nous !" ]
                    ]
                ]

        chooseFontParagraph : Element msg
        chooseFontParagraph =
            column [ width fill, spacing 32, alignTop ]
                [ paragraph
                    [ Font.size 32
                    , Font.color Color.primary
                    , Font.family [ Font.typeface "Lora" ]
                    , Region.heading 1
                    ]
                    [ text "Choisissez la police" ]
                , textColumn [ width fill, spacing 16 ]
                    [ paragraph [ spacing 8 ] [ text "Le choix d'une police est très personnel." ]
                    , paragraph [ spacing 8 ]
                        [ UI.logoWithoutEmoji 28
                        , text " vous propose un vaste choix de polices, sélectionnées une à une par nos soins."
                        ]
                    , paragraph [ spacing 8 ] [ text "Manuscrite ou en lettres d'imprimerie, vous trouverez votre bonheur !" ]
                    , paragraph [ spacing 8 ] [ text "Choisissez aussi la couleur en accord avec celle de votre enveloppe et de votre thème." ]
                    ]
                ]

        receiveEnvelopesParagraph : Element msg
        receiveEnvelopesParagraph =
            column [ width fill, spacing 32, alignTop ]
                [ paragraph
                    [ Font.size 32
                    , Font.color Color.primary
                    , Font.family [ Font.typeface "Lora" ]
                    , Region.heading 1
                    ]
                    [ text "Recevez vos enveloppes imprimées avec vos adresses" ]
                , textColumn [ width fill, spacing 16 ]
                    [ paragraph [ spacing 8 ] [ text "Vous n'avez plus qu'à mettre sous pli ! 😎" ]
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
                [ introParagraph 18 24
                , importContactParagraph
                , chooseEnvelopesParagraph
                , chooseFontParagraph
                , receiveEnvelopesParagraph
                , column [ spacing 16, centerX ]
                    [ el [ centerX ] <| text "Appelez-nous !"
                    , UI.callLink UI.callLinkAttributes
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
                [ introParagraph 24 32
                , el [ width fill ] <|
                    el
                        [ width <| maximum 400 <| fill
                        , centerX
                        , height <| px 1
                        , Background.color Color.primary
                        ]
                        none
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
                    [ textColumn [] [ paragraph [] [ text "Pour en savoir plus, contactez-nous !" ] ]
                    , row [ spacing 16 ]
                        [ link UI.callLinkAttributes
                            { url = "mailto:contact@mespetitesenveloppes.com"
                            , label = text "contact@mespetitesenveloppes.com"
                            }
                        , UI.callLink UI.callLinkAttributes
                        ]
                    ]
                ]
