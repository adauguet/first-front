module Envelope.Pricing exposing (Pricing, Step, total)


type alias Step =
    { fromQuantity : Int
    , price : Float
    }


type alias Pricing =
    ( Float, List Step )


total : Pricing -> Int -> Float
total ( base, steps ) quantity =
    toFloat quantity * rate quantity base steps


rate : Int -> Float -> List Step -> Float
rate quantity current steps =
    case steps of
        [] ->
            current

        { fromQuantity, price } :: xs ->
            if quantity < fromQuantity then
                current

            else
                rate quantity price xs
