module Envelope.Pricing exposing (Pricing, Step, total, unitPrice)


type alias Step =
    { fromQuantity : Int
    , price : Float
    }


type alias Pricing =
    ( Float, List Step )


total : Pricing -> Int -> Float
total pricing quantity =
    toFloat quantity * unitPrice pricing quantity


unitPrice : Pricing -> Int -> Float
unitPrice ( current, steps ) quantity =
    case steps of
        [] ->
            current

        { fromQuantity, price } :: xs ->
            if quantity < fromQuantity then
                current

            else
                unitPrice ( price, xs ) quantity
