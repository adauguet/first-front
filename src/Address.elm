module Address exposing (Address, make, placeholder)


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


placeholder : Address
placeholder =
    Address "Monsieur et Madame MACRON" "55 Rue du Faubourg Saint-Honoré" "" "75008" "PARIS" "FRANCE"
