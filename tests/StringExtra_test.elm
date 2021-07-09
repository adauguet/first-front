module StringExtra_test exposing (..)

import Expect exposing (equal)
import String.Extra
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The String.Extra module"
        [ describe "String.removeAllCaps"
            [ test "remove all caps" <|
                \_ -> equal (String.Extra.removeAllCaps "Monsieur et Madame MACRON") "Monsieur et Madame Macron"
            ]
        ]
