module Range_test exposing (..)

import Envelope.Pricing exposing (Pricing, Step)
import Expect exposing (FloatingPointTolerance(..), equal)
import Range exposing (Range)
import Test exposing (Test, describe, test)


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
    describe "The Range Module"
        [ describe "Range.fromPricing"
            [ test "computes the correct ranges" <|
                \_ ->
                    equal (Range.fromPricing pricing)
                        [ Range 1 (Just 49) 0.18
                        , Range 50 (Just 199) 0.16
                        , Range 200 (Just 999) 0.12
                        , Range 1000 (Just 4999) 0.11
                        , Range 5000 (Just 9999) 0.09
                        , Range 10000 Nothing 0.08
                        ]
            ]
        ]
