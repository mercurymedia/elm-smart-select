module SmartSelect.Id exposing
    ( Prefix(..)
    , option
    , select
    , container
    , input
    )


type Prefix
    = Prefix String


select : Prefix -> String
select (Prefix prefix) =
    prefix ++ "-smart-select-component"


input : Prefix -> String
input (Prefix prefix) =
    prefix ++ "-smart-select-input"


container : Prefix -> String
container (Prefix prefix) =
    prefix ++ "-smart-select-container"


option : Prefix -> Int -> String
option (Prefix prefix) idx =
    prefix ++ "-option-" ++ String.fromInt idx
