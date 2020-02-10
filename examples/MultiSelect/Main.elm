module MultiSelect.Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, text)
import Html.Attributes exposing (style, id)
import Html.Events exposing (onSubmit)
import MultiSelect


type alias Product =
    { id : Int
    , name : String
    , price : String
    }


type alias Model =
    { products : List Product
    , select : MultiSelect.SmartSelect Msg Product
    , selectedProducts : List Product
    , wasFormSubmitted : Bool
    }


type Msg
    = HandleSelectUpdate (MultiSelect.Msg Product)
    | HandleSelection ( List Product, MultiSelect.Msg Product )
    | HandleFormSubmission


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleSelectUpdate sMsg ->
            let
                ( updatedSelect, selectCmd ) =
                    MultiSelect.update sMsg model.select
            in
            ( { model | select = updatedSelect }, selectCmd )

        HandleSelection ( selection, sMsg ) ->
            let
                ( updatedSelect, selectCmd ) =
                    MultiSelect.update sMsg model.select
            in
            ( { model | selectedProducts = selection, select = updatedSelect }, selectCmd )

        HandleFormSubmission ->
            ( { model | wasFormSubmitted = True }, Cmd.none )


viewSelectedProduct : Product -> Html Msg
viewSelectedProduct product =
    div [ style "padding" ".25rem", style "cursor" "pointer", style "background-color" "#edf2f7", style "border" "1px solid #a0aec0" ]
        [ text (product.name ++ " " ++ product.price) ]


view : Model -> Html Msg
view model =
    div [ style "width" "100vw", style "height" "100vh", style "padding" "3rem" ]
        [ div [ style "margin-bottom" "1rem" ] [ text "This form contains a multi select with local search. We use a form here to demonstrate that the select key commands won't inadvertently impact form submission." ]
        , div [ id "form-submission-status", style "margin-bottom" "1rem" ]
            [ text
                (if model.wasFormSubmitted then
                    "Form submitted!"

                 else
                    "Press 'Enter' from input field or push the button below to submit form."
                )
            ]
        , form [ onSubmit HandleFormSubmission ]
            [ input [ style "margin-bottom" "1rem" ] []
            , div
                [ style "width" "500px", style "margin-bottom" "1rem" ]
                [ MultiSelect.view { selected = model.selectedProducts, options = model.products, optionLabelFn = .name, viewSelectedOptionFn = viewSelectedProduct } model.select ]
            , button [] [ text "Submit" ]
            ]
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
    ( { products = exampleProducts
      , select =
            MultiSelect.init
                { selectionMsg = HandleSelection
                , internalMsg = HandleSelectUpdate
                }
      , selectedProducts = []
      , wasFormSubmitted = False
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
