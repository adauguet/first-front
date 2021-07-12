module Font exposing (Font, all, typeface)

import Element.Font
import Envelope.Color exposing (Color(..))
import Html exposing (pre)


type alias Font =
    { name : String
    , size : Int
    , previewSize : Int
    , isAllCapsCompatible : Bool
    , style : Style
    }


type Style
    = Cursive
    | Script
    | SmallCaps


typeface : Font -> Element.Font.Font
typeface { name } =
    Element.Font.typeface name


all : ( Font, List Font )
all =
    let
        size =
            20

        previewSize =
            20
    in
    ( Font "Caveat" size previewSize True Script
    , [ Font "Annie Use Your Telescope" size previewSize True Script
      , Font "Bilbo" size previewSize True Script
      , Font "Amatic SC" size previewSize True SmallCaps
      , Font "Caveat Brush" size previewSize True Script
      , Font "Coming Soon" 16 16 True Script
      , Font "Dancing Script" size previewSize True Cursive
      , Font "Herr Von Muellerhoff" 22 24 False Cursive
      , Font "Just Me Again Down Here" size 22 True Script
      , Font "Kalam" 18 18 True Script
      , Font "Loved by the King" size previewSize True Script
      , Font "Mr De Haviland" 22 28 False Cursive
      , Font "Over the Rainbow" 18 18 False Script
      , Font "Sacramento" size previewSize False Cursive
      , Font "Sedgwick Ave" 18 18 True Script
      , Font "Stalemate" 22 26 False Script
      , Font "Sue Ellen Fransisco" size previewSize True Script
      , Font "Waiting for the Sunrise" size previewSize True Script
      , Font "Ballet" size previewSize False Cursive
      , Font "Lovers Quarrel" 24 24 False Cursive
      , Font "Miss Fajardose" 24 28 False Cursive
      , Font "Monsieur La Doulaise" size 24 False Cursive
      , Font "Mrs Saint Delafield" 22 26 False Cursive
      ]
    )
