module Range exposing (..)

import Envelope.Pricing exposing (Pricing, Step)


type alias Range =
    { from : Int
    , to : Maybe Int
    , price : Float
    }


fromPricing : Pricing -> List Range
fromPricing pricing =
    compute 1 pricing []


compute : Int -> ( Float, List Step ) -> List Range -> List Range
compute current ( p, steps ) ranges =
    case steps of
        [] ->
            Range current Nothing p :: ranges

        { fromQuantity, price } :: xs ->
            Range current (Just (fromQuantity - 1)) p :: compute fromQuantity ( price, xs ) ranges
