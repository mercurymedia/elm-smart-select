module SingleSelect.Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import SmartSelect.Utilities exposing (SearchUnion(..))
import SmartSelectSingle as SingleSelect


type alias Product =
    { id : Int
    , name : String
    , price : String
    }


type alias Model =
    { singleSelect : SingleSelect.SmartSelect Msg Product

    -- other fields...
    }


singleSelectSettings : List Product -> SingleSelect.Settings Msg Product
singleSelectSettings products =
    { internalMsg = \msg -> HandleSingleSelectUpdate msg
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
    , closeOnSelect = True
    }


type Msg
    = HandleSingleSelectUpdate (SingleSelect.Msg Product)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleSingleSelectUpdate sMsg ->
            let
                ( updatedSelect, selectCmd ) =
                    SingleSelect.update sMsg model.singleSelect
            in
            ( { model | singleSelect = updatedSelect }, selectCmd )


view : Model -> Html Msg
view model =
    div [ style "width" "100vw", style "height" "100vh", style "padding" "3rem" ]
        [ div [ style "padding-bottom" "1rem" ] [ text "This is a single SmartSelect" ]
        , div
            [ style "width" "500px" ]
            [ SingleSelect.view False model.singleSelect ]
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
    ( { singleSelect = SingleSelect.init (singleSelectSettings exampleProducts) Nothing }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ SingleSelect.subscriptions model.singleSelect ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
