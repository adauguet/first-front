module Envelope exposing (Envelope, references)

import Envelope.Color exposing (Color(..))
import Envelope.Format exposing (Format(..))
import Envelope.Pricing exposing (Pricing, Step)


type alias Envelope =
    { format : Format
    , color : Color
    , pricing : Pricing
    }


references : List Envelope
references =
    [ { format = Square 130
      , color = Ivory
      , pricing =
            ( 0.38
            , [ Step 50 0.34
              , Step 200 0.26
              , Step 1000 0.24
              ]
            )
      }
    , { format = Square 130
      , color = White
      , pricing =
            ( 0.2
            , [ Step 50 0.16
              , Step 200 0.12
              ]
            )
      }
    , { format = Square 130
      , color = Cream
      , pricing =
            ( 0.36
            , [ Step 50 0.32
              , Step 200 0.24
              , Step 1000 0.22
              ]
            )
      }
    , { format = Square 140
      , color = Ivory
      , pricing =
            ( 0.44
            , [ Step 50 0.38
              , Step 200 0.3
              , Step 1000 0.26
              ]
            )
      }
    , { format = Square 140
      , color = White
      , pricing =
            ( 0.26
            , [ Step 50 0.24
              , Step 200 0.2
              , Step 1000 0.16
              ]
            )
      }
    ]
