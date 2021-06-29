module Pricing_test exposing (..)

import Expect exposing (FloatingPointTolerance(..), within)
import Fuzz exposing (intRange)
import Pricing exposing (Pricing, Step, total)
import Test exposing (Test, describe, fuzz)


pricing : Pricing
pricing =
    ( 0.18
    , [ Step 50 0.16
      , Step 200 0.12
      , Step 1000 0.11
      , Step 5000 0.09
      , Step 10000 0.08
      ]
    )


suite : Test
suite =
    describe "The Pricing module"
        [ describe "Pricing.total"
            [ fuzz (intRange 1 49) "computes the right amount for a quantity between 1 and 49" <|
                \quantity -> within (Absolute 0) (total quantity pricing) (toFloat quantity * 0.18)
            , fuzz (intRange 50 199) "computes the right amount for a quantity between 50 and 199" <|
                \quantity -> within (Absolute 0) (total quantity pricing) (toFloat quantity * 0.16)
            , fuzz (intRange 200 999) "computes the right amount for a quantity between 200 and 999" <|
                \quantity -> within (Absolute 0) (total quantity pricing) (toFloat quantity * 0.12)
            , fuzz (intRange 1000 4999) "computes the right amount for a quantity between 1000 and 4999" <|
                \quantity -> within (Absolute 0) (total quantity pricing) (toFloat quantity * 0.11)
            , fuzz (intRange 5000 9999) "computes the right amount for a quantity between 5000 and 9999" <|
                \quantity -> within (Absolute 0) (total quantity pricing) (toFloat quantity * 0.09)
            , fuzz (intRange 10000 100000) "computes the right amount for a quantity between 10000 and 100000" <|
                \quantity -> within (Absolute 0) (total quantity pricing) (toFloat quantity * 0.08)
            ]
        ]
