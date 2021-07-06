module Address exposing (Address, make)


type alias Address =
    { name : String
    , street : String
    , streetLine2 : String
    , postalCode : String
    , city : String
    , country : String
    }


make : List String -> Maybe Address
make list =
    case list of
        name :: street :: streetLine2 :: postalCode :: city :: country :: _ ->
            Just <| Address name street streetLine2 postalCode city country

        _ ->
            Nothing
