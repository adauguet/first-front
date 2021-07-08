module Millimeter exposing
    ( Millimeter
    , cm
    , mm
    , toFloat
    , toString
    )


mm : Int -> Millimeter
mm =
    Millimeter


cm : Int -> Millimeter
cm int =
    Millimeter (int * 10)


type Millimeter
    = Millimeter Int


toString : Millimeter -> String
toString (Millimeter int) =
    String.fromInt int


toFloat : Millimeter -> Float
toFloat (Millimeter int) =
    Basics.toFloat int
