module MultiSelectRemoteExample exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, button, div, form, input, p, text, h1, a)
import Html.Attributes exposing (id, style, target, href)
import Html.Events exposing (onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import MultiSelectRemote


type alias Language =
    { name : String
    }


type alias Model =
    { select : MultiSelectRemote.SmartSelect Msg Language
    , selectedOptions : List Language
    , wasFormSubmitted : Bool
    }


type Msg
    = HandleFormSubmission
    | GotOptionSelected ( List Language, MultiSelectRemote.Msg Language )
    | SelectUpdated (MultiSelectRemote.Msg Language)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotOptionSelected ( selectedOptions, sMsg )  ->
          let
            ( updatedSelect, selectCmd ) =
                MultiSelectRemote.update sMsg httpRemoteSearchAttrs model.select
          in
          ( { model | select = updatedSelect, selectedOptions = selectedOptions }, selectCmd )

        SelectUpdated sMsg ->
          let
            ( updatedSelect, selectCmd ) =
                MultiSelectRemote.update sMsg httpRemoteSearchAttrs model.select
          in
          ( { model | select = updatedSelect }, selectCmd )

        HandleFormSubmission ->
            ( { model | wasFormSubmitted = True }, Cmd.none )

viewSelectedLanguage : Language -> Html Msg
viewSelectedLanguage lang =
    div [ style "padding" ".25rem", style "cursor" "pointer", style "background-color" "#edf2f7", style "border" "1px solid #a0aec0" ]
        [ text lang.name ]


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
            [ text "This form contains a multi select with local search. We use a form here to demonstrate that the select key commands won't inadvertently impact form submission." ]
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
                [ MultiSelectRemote.view { selected = model.selectedOptions, optionLabelFn = .name, viewSelectedOptionFn = viewSelectedLanguage } model.select ]
            , button [] [ text "Submit" ]
            ]
        ]


init : ( Model, Cmd Msg )
init =
    ( { select =
          MultiSelectRemote.init
            { characterSearchThreshold = 2
            , debounceDuration = 1000
            , selectionMsg = GotOptionSelected
            , internalMsg = SelectUpdated
            , idPrefix = "multi-select-remote"
            }
      , selectedOptions = []
      , wasFormSubmitted = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    MultiSelectRemote.subscriptions model.select


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
