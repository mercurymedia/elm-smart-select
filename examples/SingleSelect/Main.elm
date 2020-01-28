module SingleSelect.Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onSubmit)
import SingleSelect


type alias Product =
    { id : Int
    , name : String
    , price : String
    }


type alias Model =
    { products : List Product
    , select : SingleSelect.SmartSelect Msg Product
    , selectedProduct : Maybe Product
    , wasFormSubmitted : Bool
    }


type Msg
    = HandleSelectUpdate (SingleSelect.Msg Product)
    | HandleSelection ( Product, SingleSelect.Msg Product )
    | HandleFormSubmission


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleSelectUpdate sMsg ->
            let
                ( updatedSelect, selectCmd ) =
                    SingleSelect.update sMsg model.select
            in
            ( { model | select = updatedSelect }, selectCmd )

        HandleSelection ( selection, sMsg ) ->
            let
                ( updatedSelect, selectCmd ) =
                    SingleSelect.update sMsg model.select
            in
            ( { model | selectedProduct = Just selection, select = updatedSelect }, selectCmd )

        HandleFormSubmission ->
            ( { model | wasFormSubmitted = True }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ style "width" "100vw", style "height" "100vh", style "padding" "3rem" ]
        [ div [ style "margin-bottom" "1rem" ] [ text "This form contains a single select with local search. We use a form here to demonstrate that the select key commands won't inadvertently impact form submission." ]
        , div [ style "margin-bottom" "1rem" ]
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
                [ SingleSelect.view { selected = model.selectedProduct, options = model.products, optionLabelFn = .name } model.select ]
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
            SingleSelect.init
                { selectionMsg = HandleSelection
                , internalMsg = HandleSelectUpdate
                }
      , selectedProduct = Nothing
      , wasFormSubmitted = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    SingleSelect.subscriptions model.select


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
