module SingleSelectRemoteExample exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, a, button, div, form, h1, input, p, text)
import Html.Attributes exposing (href, id, style, target)
import Html.Events exposing (onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import SingleSelectRemote
import SmartSelect.Settings exposing (defaultSettings)


type alias Language =
    { name : String
    }


type alias Model =
    { select : SingleSelectRemote.SmartSelect Msg Language
    , selectedOption : Maybe Language
    , wasFormSubmitted : Bool
    }


type Msg
    = HandleFormSubmission
    | GotOptionSelected ( Language, SingleSelectRemote.Msg Language )
    | SelectUpdated (SingleSelectRemote.Msg Language)
    | OnViewChange


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotOptionSelected ( selectedOption, sMsg ) ->
            let
                ( updatedSelect, selectCmd ) =
                    SingleSelectRemote.update sMsg httpRemoteSearchAttrs model.select
            in
            ( { model | select = updatedSelect, selectedOption = Just selectedOption }, selectCmd )

        SelectUpdated sMsg ->
            let
                ( updatedSelect, selectCmd ) =
                    SingleSelectRemote.update sMsg httpRemoteSearchAttrs model.select
            in
            ( { model | select = updatedSelect }, selectCmd )

        HandleFormSubmission ->
            ( { model | wasFormSubmitted = True }, Cmd.none )

        OnViewChange ->
            let
                ( updatedSelect, selectCmd ) =
                    SingleSelectRemote.updatePosition model.select
            in
            ( { model | select = updatedSelect }, selectCmd )


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "height" "100vh"
        , style "padding" "3rem"
        , style "overflow" "auto"
        , Html.Events.on "scroll" (Decode.succeed OnViewChange)
        ]
        [ h1 [] [ text "SingleSelectRemote Example" ]
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
                [ SingleSelectRemote.view
                    { selected = model.selectedOption
                    , optionLabelFn = .name
                    , settings = defaultSettings
                    }
                    model.select
                ]
            , button [] [ text "Submit" ]
            ]
        , div [ style "height" "100vh" ] []
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


httpRemoteSearchAttrs : { headers : List Http.Header, url : String -> String, optionDecoder : Decoder Language }
httpRemoteSearchAttrs =
    { headers = []
    , url = \param -> "https://freetestapi.com/api/v1/languages?search=" ++ param
    , optionDecoder = optionDecoder
    }


optionDecoder : Decoder Language
optionDecoder =
    Decode.map (\name -> { name = name })
        (Decode.field "name" Decode.string)
