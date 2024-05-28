module SingleSelectRemoteExample exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, button, div, form, input, p, text, h1, a)
import Html.Attributes exposing (id, style, target, href)
import Html.Events exposing (onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import SingleSelectRemote


type alias Product =
    { name : String
    }


type alias Model =
    { select : SingleSelectRemote.SmartSelect Msg Product
    , selectedOption : Maybe Product
    , wasFormSubmitted : Bool
    }


type Msg
    = HandleFormSubmission
    | GotOptionSelected ( Maybe Product, SingleSelectRemote.Msg Product )
    | SelectUpdated (SingleSelectRemote.Msg Product)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotOptionSelected ( selectedOption, sMsg )  ->
          let
            ( updatedSelect, selectCmd ) =
                SingleSelectRemote.update sMsg httpRemoteSearchAttrs model.select
          in
          ( { model | select = updatedSelect, selectedOption = selectedOption }, selectCmd )

        SelectUpdated sMsg ->
          let
            ( updatedSelect, selectCmd ) =
                SingleSelectRemote.update sMsg httpRemoteSearchAttrs model.select
          in
          ( { model | select = updatedSelect }, selectCmd )

        HandleFormSubmission ->
            ( { model | wasFormSubmitted = True }, Cmd.none )

          


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "height" "100vh"
        , style "padding" "3rem"
        ]
        [ h1 [] [ text "SingleSelectRemote Example"]
        , div
            [ style "margin-bottom" "1rem"
            ]
            [ text "This form contains a single select with local search. We use a form here to demonstrate that the select key commands won't inadvertently impact form submission." ]
        , div [ id "form-submission-status", style "margin-bottom" "1rem" ]
            [ text
                (if model.wasFormSubmitted then
                    "Form submitted!"

                 else
                    "Press 'Enter' from input field or push the button below to submit form."
                )
            ]
        , form [ onSubmit HandleFormSubmission ]
            [ input [ style "margin-bottom" "2rem" ] []
            , p [] 
                [ text "Search for languages from "
                , a [ style "color" "#3182ce", target "_blank", href "https://freetestapi.com/apis/languages" ] 
                    [ text "https://freetestapi.com/apis/languages" ]
                , text "."
                ]
            , div
                [ style "width" "500px", style "margin-bottom" "1rem" ]
                [ SingleSelectRemote.view { selected = model.selectedOption, optionLabelFn = .name } model.select ]
            , button [] [ text "Submit" ]
            ]
        ]


init : ( Model, Cmd Msg )
init =
    ( { select =
          SingleSelectRemote.init
            { characterSearchThreshold = 2
            , debounceDuration = 1000
            , selectionMsg = GotOptionSelected
            , internalMsg = SelectUpdated
            , idPrefix = "single-select-remote"
            }
      , selectedOption = Nothing
      , wasFormSubmitted = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    SingleSelectRemote.subscriptions model.select


httpRemoteSearchAttrs : { headers : List Http.Header, url : String -> String, optionDecoder : Decoder Product }
httpRemoteSearchAttrs =
    { headers = []
    , url = \param -> "https://freetestapi.com/api/v1/languages?search=" ++ param
    , optionDecoder = optionDecoder
    }

optionDecoder : Decoder Product
optionDecoder =
    Decode.map (\name -> { name = name })
      (Decode.field "name" Decode.string)
