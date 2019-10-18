module MultiSelect.Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import MultiSelect


type alias Product =
    { id : Int
    , name : String
    , price : String
    }


type alias Model =
    { select : MultiSelect.SmartSelect Msg Product
    , products : List Product
    }


type Msg
    = HandleSelectUpdate (MultiSelect.Msg Product)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleSelectUpdate sMsg ->
            let
                ( updatedSelect, selectCmd ) =
                    MultiSelect.update sMsg model.select
            in
            ( { model | select = updatedSelect }, selectCmd )


viewSelectedProduct : Product -> Html Msg
viewSelectedProduct product =
    div [ style "padding" ".25rem", style "cursor" "pointer", style "background-color" "#edf2f7", style "border" "1px solid #a0aec0" ]
        [ text (product.name ++ " " ++ product.price) ]


view : Model -> Html Msg
view model =
    div [ style "width" "100vw", style "height" "100vh", style "padding" "3rem" ]
        [ div [ style "padding-bottom" "1rem" ] [ text "This is a multi select with local search" ]
        , div
            [ style "width" "500px" ]
            [ MultiSelect.view { options = model.products, optionLabelFn = .name, viewSelectedOptionFn = viewSelectedProduct } model.select ]
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
    ( { select = MultiSelect.init (\msg -> HandleSelectUpdate msg)
      , products = exampleProducts
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    MultiSelect.subscriptions model.select


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
