module MultiSelect.Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import SmartSelect.Utilities exposing (SearchUnion(..))
import SmartSelectMulti as MultiSelect


type alias Product =
    { id : Int
    , name : String
    , price : String
    }


type alias Model =
    { multiSelect : MultiSelect.SmartSelect Msg Product

    -- other fields...
    }


multiSelectSettings : List Product -> MultiSelect.Settings Msg Product
multiSelectSettings products =
    { internalMsg = \msg -> HandleMultiSelectUpdate msg
    , searchFn =
        Local
            (\searchText ->
                if searchText == "" then
                    products

                else
                    List.filter (\product -> String.contains (String.toLower searchText) (String.toLower product.name)) products
            )
    , optionType = "Product"
    , optionLabel = \product -> product.name
    , optionDescription = \product -> product.price
    , debounceDuration = 0
    , characterSearchThreshold = 0
    , closeOnSelect = False
    }


type Msg
    = HandleMultiSelectUpdate (MultiSelect.Msg Product)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleMultiSelectUpdate sMsg ->
            let
                ( updatedSelect, selectCmd ) =
                    MultiSelect.update sMsg model.multiSelect
            in
            ( { model | multiSelect = updatedSelect }, selectCmd )


viewSelectedProduct : Product -> Html Msg
viewSelectedProduct product =
    div [ style "padding" ".25rem", style "cursor" "pointer", style "background-color" "#edf2f7", style "border" "1px solid #a0aec0" ]
        [ text (product.name ++ " " ++ product.price) ]


view : Model -> Html Msg
view model =
    div [ style "width" "100vw", style "height" "100vh", style "padding" "3rem" ]
        [ div [ style "padding-bottom" "1rem" ] [ text "This is a multi SmartSelect" ]
        , div
            [ style "width" "500px", style "display" "inline-flex" ]
            [ MultiSelect.view False viewSelectedProduct model.multiSelect ]
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
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { multiSelect = MultiSelect.init (multiSelectSettings exampleProducts) [] }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ MultiSelect.subscriptions model.multiSelect
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
