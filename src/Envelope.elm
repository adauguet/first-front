module Envelope exposing (Envelope, references)

import Envelope.Color exposing (Color(..))
import Envelope.Format exposing (Format(..))
import Envelope.Pricing exposing (Pricing, Step)
import Millimeter exposing (mm)


type alias Envelope =
    { format : Format
    , color : Color
    , pricing : Pricing
    }


references : ( Envelope, List Envelope )
references =
    ( { format = Square (mm 130), color = Ivory, pricing = ( 0.38, [ Step 50 0.34, Step 200 0.26, Step 1000 0.24 ] ) }
    , [ { format = Square (mm 130), color = White, pricing = ( 0.2, [ Step 50 0.16, Step 200 0.12 ] ) }
      , { format = Square (mm 130), color = Cream, pricing = ( 0.36, [ Step 50 0.32, Step 200 0.24, Step 1000 0.22 ] ) }
      , { format = Square (mm 140), color = Ivory, pricing = ( 0.44, [ Step 50 0.38, Step 200 0.3, Step 1000 0.26 ] ) }
      , { format = Square (mm 140), color = White, pricing = ( 0.26, [ Step 50 0.24, Step 200 0.2, Step 1000 0.16 ] ) }
      , { format = Square (mm 155), color = Ivory, pricing = ( 0.44, [ Step 50 0.38, Step 200 0.32, Step 1000 0.26 ] ) }
      , { format = Square (mm 155), color = White, pricing = ( 0.22, [ Step 50 0.2, Step 200 0.16, Step 1000 0.14 ] ) }
      , { format = Square (mm 155), color = Flecked, pricing = ( 0.32, [ Step 50 0.28, Step 200 0.22, Step 1000 0.2 ] ) }
      , { format = Square (mm 155), color = Fuchsia, pricing = ( 0.38, [ Step 50 0.36, Step 200 0.26, Step 1000 0.24 ] ) }
      , { format = Square (mm 155), color = BlueKingFisher, pricing = ( 0.48, [ Step 50 0.44, Step 200 0.34, Step 1000 0.28 ] ) }
      , { format = Rectangle (mm 114) (mm 162), color = Ivory, pricing = ( 0.36, [ Step 50 0.32, Step 200 0.24, Step 1000 0.22 ] ) }
      , { format = Rectangle (mm 114) (mm 162), color = White, pricing = ( 0.16, [ Step 200 0.12 ] ) }
      , { format = Rectangle (mm 114) (mm 162), color = Flecked, pricing = ( 0.28, [ Step 50 0.26, Step 200 0.2, Step 1000 0.16 ] ) }
      , { format = Rectangle (mm 114) (mm 162), color = Fuchsia, pricing = ( 0.34, [ Step 50 0.32, Step 200 0.24, Step 1000 0.22 ] ) }
      ]
    )
