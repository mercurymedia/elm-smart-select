module SingleSelectLocal.Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import SingleSelectLocal as Select


type alias Product =
    { id : Int
    , name : String
    , price : String
    }


type alias Model =
    { select : Select.SmartSelect Msg Product
    , products : List Product
    }


selectSettings : Select.Settings Msg Product
selectSettings =
    { internalMsg = \msg -> HandleSelectUpdate msg
    , searchFn =
        \searchText products ->
            List.filter (\product -> String.contains (String.toLower searchText) (String.toLower product.name)) products
    , optionType = "Product"
    , optionLabel = \product -> product.name
    , optionDescription = \product -> product.price
    , optionsContainerMaxHeight = 300
    , optionHeight = 50
    , closeOnSelect = True
    }


type Msg
    = HandleSelectUpdate (Select.Msg Product)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleSelectUpdate sMsg ->
            let
                ( updatedSelect, selectCmd ) =
                    Select.update sMsg model.select
            in
            ( { model | select = updatedSelect }, selectCmd )


view : Model -> Html Msg
view model =
    div [ style "width" "100vw", style "height" "100vh", style "padding" "3rem" ]
        [ div [ style "padding-bottom" "1rem" ] [ text "This is a single select with local search" ]
        , div
            [ style "width" "500px" ]
            [ Select.view False model.products model.select ]
        ]


exampleProducts : List Product
exampleProducts =
    [ { id = 1
      , name = "product 1"
      , price = "$3.00"
      }
    , { id = 2
      , name = "product 2"
      , price = "$5.00"
      }
    , { id = 3
      , name = "product 3"
      , price = "$7.00"
      }
    , { id = 4
      , name = "product 4"
      , price = "$3.00"
      }
    , { id = 5
      , name = "product 5"
      , price = "$5.00"
      }
    , { id = 6
      , name = "product 6"
      , price = "$7.00"
      }
    , { id = 7
      , name = "product 7"
      , price = "$3.00"
      }
    , { id = 8
      , name = "product 8"
      , price = "$5.00"
      }
    , { id = 9
      , name = "product 9"
      , price = "$7.00"
      }
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { select = Select.init selectSettings Nothing, products = exampleProducts }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Select.subscriptions model.select ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
