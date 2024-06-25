module SingleSelectRemote exposing (SmartSelect, Msg, init, view, viewCustom, subscriptions, update)

{-| A select component for a single selection with remote data.


# Architecture

@docs SmartSelect, Msg, init, view, viewCustom, subscriptions, update

-}

import Browser.Dom as Dom
import Browser.Events
import Color
import Debounce exposing (Debounce)
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (autocomplete, class, classList, id, placeholder, style, value)
import Html.Events as Events exposing (onClick, onInput, onMouseEnter)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..))
import SmartSelect.Alignment as Alignment exposing (Alignment)
import SmartSelect.Errors as Errors
import SmartSelect.Id as Id exposing (Prefix(..))
import SmartSelect.Utilities as Utilities exposing (KeyCode(..), RemoteQueryAttrs)
import SmartSelect.ViewComponents exposing (classPrefix, viewEmptyOptionsListItem, viewError, viewOptionsList, viewOptionsListItem, viewSpinner, viewTextField, viewTextFieldContainer)
import Spinner


{-| The opaque type representing a particular smart select instance.
-}
type SmartSelect msg a
    = SmartSelect (Model msg a)


type alias Model msg a =
    { selectWidth : Float
    , isOpen : Bool
    , searchText : String
    , debounce : Debounce String
    , spinner : Spinner.Model
    , remoteData : RemoteData ( String, String ) (List a)
    , focusedOptionIndex : Int
    , selectionMsg : ( Maybe a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , characterSearchThreshold : Int
    , debounceDuration : Float
    , idPrefix : Prefix
    , alignment : Maybe Alignment
    }



-- type Prefix
--     = Prefix String


{-| Opaque type representing cases to be passed to SingleSelectRemote.update
-}
type Msg a
    = NoOp
    | SetFocused Int
    | UpKeyPressed Int
    | DownKeyPressed Int
    | SetSearchText String
    | DebounceMsg Debounce.Msg
    | SpinnerMsg Spinner.Msg
    | GotRemoteData (RemoteData Http.Error (List a))
    | WindowResized ( Int, Int )
    | DismissError
    | Open
    | GotAlignment (Result Dom.Error Alignment)
    | Close
    | Clear


{-| Instantiates and returns a smart select.

  - `selectionMsg` takes a function that expects a tuple representing the selection and a SinglSelectRemote.Msg msg and returns an externally defined msg for handling selection.
  - `internalMsg` takes a function that expects a SingleSelectRemote.Msg and returns an externally defined msg.
  - `characterSearchThreshold` takes an integer that specifies how many characters need to be typed before triggering the remote query.
  - `debounceDuration` takes a float that specifies the duration in milliseconds between the last keypress and remote query being triggered.
  - `idPrefix` takes a string with a unique prefix

-}
init : { selectionMsg : ( Maybe a, Msg a ) -> msg, internalMsg : Msg a -> msg, characterSearchThreshold : Int, debounceDuration : Float, idPrefix : String } -> SmartSelect msg a
init { selectionMsg, internalMsg, characterSearchThreshold, debounceDuration, idPrefix } =
    SmartSelect
        { selectWidth = 0
        , isOpen = False
        , searchText = ""
        , debounce = Debounce.init
        , spinner = Spinner.init
        , remoteData = NotAsked
        , focusedOptionIndex = 0
        , selectionMsg = selectionMsg
        , internalMsg = internalMsg
        , characterSearchThreshold = characterSearchThreshold
        , debounceDuration = debounceDuration
        , idPrefix = Prefix idPrefix
        , alignment = Nothing
        }


{-| Events external to the smart select to which it is subscribed.
-}
subscriptions : SmartSelect msg a -> Sub msg
subscriptions (SmartSelect model) =
    if model.isOpen then
        Sub.batch
            [ Browser.Events.onResize (\h w -> model.internalMsg <| WindowResized ( h, w ))
            , case model.remoteData of
                NotAsked ->
                    if model.characterSearchThreshold == 0 then
                        Sub.map (\sMsg -> model.internalMsg <| SpinnerMsg sMsg) Spinner.subscription

                    else
                        Sub.none

                Loading ->
                    Sub.map (\sMsg -> model.internalMsg <| SpinnerMsg sMsg) Spinner.subscription

                _ ->
                    Sub.none
            , Browser.Events.onMouseDown (Utilities.clickedOutsideSelect (Id.select model.idPrefix) (model.internalMsg Close))
            ]

    else
        Sub.none


keyActionMapper : { remoteData : RemoteData ( String, String ) (List a), focusedOptionIndex : Int, selectionMsg : ( Maybe a, Msg a ) -> msg, internalMsg : Msg a -> msg } -> Decode.Decoder ( msg, Bool )
keyActionMapper { remoteData, focusedOptionIndex, selectionMsg, internalMsg } =
    let
        options =
            case remoteData of
                Success opts ->
                    indexOptions opts

                _ ->
                    []
    in
    Decode.field "key" Decode.string
        |> Decode.map Utilities.toKeyCode
        |> Decode.map
            (\key ->
                case key of
                    Up ->
                        let
                            newIdx =
                                if focusedOptionIndex - 1 < 0 then
                                    0

                                else
                                    focusedOptionIndex - 1
                        in
                        ( internalMsg <| UpKeyPressed newIdx, Utilities.preventDefault key )

                    Down ->
                        let
                            newIdx =
                                if focusedOptionIndex + 1 > (List.length options - 1) then
                                    List.length options - 1

                                else
                                    focusedOptionIndex + 1
                        in
                        ( internalMsg <| DownKeyPressed newIdx, Utilities.preventDefault key )

                    Enter ->
                        case Dict.get focusedOptionIndex (Dict.fromList options) of
                            Just item ->
                                ( selectionMsg ( Just item, Close ), Utilities.preventDefault key )

                            Nothing ->
                                ( internalMsg NoOp, Utilities.preventDefault key )

                    Escape ->
                        ( internalMsg Close, Utilities.preventDefault key )

                    _ ->
                        ( internalMsg NoOp, Utilities.preventDefault key )
            )


debounceConfig : { internalMsg : Msg a -> msg, debounceDuration : Float } -> Debounce.Config msg
debounceConfig { internalMsg, debounceDuration } =
    { strategy = Debounce.later debounceDuration
    , transform = \debounceMsg -> internalMsg <| DebounceMsg debounceMsg
    }


{-| Update the provided smart select and receive the updated select instance and a cmd to run.

    type alias RemoteSearchAttrs a =
        { headers : List Header
        , url : String -> String
        , optionDecoder : Decoder a
        }

-}
update : Msg a -> RemoteQueryAttrs a -> SmartSelect msg a -> ( SmartSelect msg a, Cmd msg )
update msg remoteQueryAttrs (SmartSelect model) =
    case msg of
        NoOp ->
            ( SmartSelect model, Cmd.none )

        SetFocused idx ->
            ( SmartSelect { model | focusedOptionIndex = idx }, Cmd.none )

        UpKeyPressed idx ->
            ( SmartSelect { model | focusedOptionIndex = idx }, Utilities.scrollToOption (model.internalMsg NoOp) model.idPrefix idx )

        DownKeyPressed idx ->
            ( SmartSelect { model | focusedOptionIndex = idx }, Utilities.scrollToOption (model.internalMsg NoOp) model.idPrefix idx )

        SetSearchText text ->
            if String.length text < model.characterSearchThreshold then
                ( SmartSelect { model | searchText = text, remoteData = NotAsked }, Cmd.none )

            else
                let
                    ( debounce, cmd ) =
                        Debounce.push (debounceConfig { internalMsg = model.internalMsg, debounceDuration = model.debounceDuration }) text model.debounce
                in
                ( SmartSelect { model | searchText = text, debounce = debounce }
                , cmd
                )

        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        (debounceConfig { internalMsg = model.internalMsg, debounceDuration = model.debounceDuration })
                        (Debounce.takeLast (Utilities.search { remoteQueryAttrs = remoteQueryAttrs, handleResponse = \remoteData -> model.internalMsg <| GotRemoteData remoteData }))
                        msg_
                        model.debounce
            in
            ( SmartSelect { model | debounce = debounce, remoteData = Loading }, cmd )

        SpinnerMsg spinnerMsg ->
            let
                spinnerModel =
                    Spinner.update spinnerMsg model.spinner
            in
            ( SmartSelect { model | spinner = spinnerModel }
            , Cmd.none
            )

        GotRemoteData data ->
            ( SmartSelect { model | focusedOptionIndex = 0, remoteData = RemoteData.mapError (Errors.httpErrorToReqErrTuple "GET") data }, Cmd.none )

        WindowResized _ ->
            ( SmartSelect model, Alignment.getAlignment model.idPrefix (\alignment -> model.internalMsg (GotAlignment alignment)) )

        GotAlignment result ->
            case result of
                Ok alignment ->
                    ( SmartSelect { model | alignment = Just alignment }, Cmd.none )

                Err _ ->
                    ( SmartSelect model, Cmd.none )

        DismissError ->
            case model.remoteData of
                Failure _ ->
                    ( SmartSelect { model | remoteData = NotAsked }, Cmd.none )

                _ ->
                    ( SmartSelect model, Cmd.none )

        Open ->
            let
                cmd =
                    if model.characterSearchThreshold == 0 then
                        Utilities.search { remoteQueryAttrs = remoteQueryAttrs, handleResponse = \remoteData -> model.internalMsg <| GotRemoteData remoteData } ""

                    else
                        Cmd.none

                ( updatedModel, popoverCmd ) =
                    openPopover (SmartSelect model)
            in
            ( updatedModel, Cmd.batch [ popoverCmd, cmd ] )

        Close ->
            ( SmartSelect { model | isOpen = False, searchText = "", alignment = Nothing, remoteData = NotAsked }
            , Utilities.blurInput model.idPrefix (model.internalMsg NoOp)
            )

        Clear ->
            openPopover (SmartSelect { model | remoteData = NotAsked })


openPopover : SmartSelect msg a -> ( SmartSelect msg a, Cmd msg )
openPopover (SmartSelect model) =
    ( SmartSelect { model | isOpen = True, searchText = "", focusedOptionIndex = 0 }
    , Cmd.batch
        [ Alignment.getAlignment model.idPrefix (\alignment -> model.internalMsg (GotAlignment alignment))
        , Utilities.focusInput model.idPrefix (model.internalMsg NoOp)
        ]
    )


showOptions :
    { selectionMsg : ( Maybe a, Msg a ) -> msg
    , selectedOption : Maybe a
    , internalMsg : Msg a -> msg
    , focusedOptionIndex : Int
    , searchText : String
    , options : List ( Int, a )
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    , idPrefix : Prefix
    }
    -> Html msg
showOptions { selectionMsg, selectedOption, internalMsg, focusedOptionIndex, searchText, options, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight, noResultsForMsg, noOptionsMsg, idPrefix } =
    viewOptionsList
        [ id (Id.container idPrefix)
        , style "max-height" (String.fromFloat optionsContainerMaxHeight ++ "px")
        ]
        (if List.isEmpty options && searchText /= "" then
            [ viewEmptyOptionsListItem [] [ text <| noResultsForMsg searchText ] ]

         else if List.isEmpty options then
            [ viewEmptyOptionsListItem [] [ text noOptionsMsg ] ]

         else
            List.map
                (\( idx, option ) ->
                    let
                        isSelected =
                            Maybe.map (\value -> value == option) selectedOption
                                |> Maybe.withDefault False
                    in
                    viewOptionsListItem
                        [ Events.stopPropagationOn "click" (Decode.succeed ( selectionMsg ( Just option, Close ), True ))
                        , onMouseEnter <| internalMsg <| SetFocused idx
                        , id <| Id.option idPrefix idx
                        ]
                        { label = optionLabelFn option
                        , description = optionDescriptionFn option
                        , isFocused = idx == focusedOptionIndex
                        , isSelected = isSelected
                        }
                )
                options
        )


viewRemoteData :
    { selectionMsg : ( Maybe a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , focusedOptionIndex : Int
    , characterSearchThreshold : Int
    , searchText : String
    , selectedOption : Maybe a
    , remoteData : RemoteData ( String, String ) (List a)
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    , spinner : Spinner.Model
    , spinnerColor : Color.Color
    , characterThresholdPrompt : Int -> String
    , queryErrorMsg : String
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    , idPrefix : Prefix
    }
    -> Html msg
viewRemoteData { selectionMsg, internalMsg, focusedOptionIndex, characterSearchThreshold, searchText, selectedOption, remoteData, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight, spinner, spinnerColor, characterThresholdPrompt, queryErrorMsg, noResultsForMsg, noOptionsMsg, idPrefix } =
    case remoteData of
        NotAsked ->
            if characterSearchThreshold == 0 then
                viewSpinner { spinner = spinner, spinnerColor = spinnerColor }

            else
                let
                    difference =
                        characterSearchThreshold - String.length searchText

                    searchPrompt =
                        if difference == 0 then
                            viewSpinner { spinner = spinner, spinnerColor = spinnerColor }

                        else
                            div [ class (classPrefix "search-prompt") ] [ text <| characterThresholdPrompt difference ]
                in
                div [ class (classPrefix "search-prompt-container") ] [ searchPrompt ]

        Loading ->
            viewSpinner { spinner = spinner, spinnerColor = spinnerColor }

        Success options ->
            showOptions
                { selectionMsg = selectionMsg
                , selectedOption = selectedOption
                , internalMsg = internalMsg
                , focusedOptionIndex = focusedOptionIndex
                , searchText = searchText
                , options = indexOptions options
                , optionLabelFn = optionLabelFn
                , optionDescriptionFn = optionDescriptionFn
                , optionsContainerMaxHeight = optionsContainerMaxHeight
                , noResultsForMsg = noResultsForMsg
                , noOptionsMsg = noOptionsMsg
                , idPrefix = idPrefix
                }

        Failure _ ->
            viewError []
                { message = queryErrorMsg, onDismiss = internalMsg DismissError }


indexOptions : List a -> List ( Int, a )
indexOptions options =
    List.indexedMap Tuple.pair options


{-| The smart select view for selecting one option at a time with remote data.

  - `selected` takes the currently selected entity, if any.
  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".

-}
view : { selected : Maybe a, optionLabelFn : a -> String } -> SmartSelect msg a -> Html msg
view { selected, optionLabelFn } smartSelect =
    let
        config =
            { isDisabled = False
            , selected = selected
            , optionLabelFn = optionLabelFn
            , optionDescriptionFn = \_ -> ""
            , optionsContainerMaxHeight = 300
            , spinnerColor = Color.rgb255 57 179 181
            , selectTitle = ""
            , searchPrompt = "Placeholder..."
            , characterThresholdPrompt = \_ -> "Enter at least 2 characters to search.."
            , queryErrorMsg = "Error"
            , noResultsForMsg = \_ -> "No results"
            , noOptionsMsg = "No Options"
            }
    in
    viewCustom config smartSelect


{-| The customizable smart select view for selecting one option at a time with remote data. It expects the following arguments (in order):

  - `isDisabled` takes a boolean that indicates whether or not the select can be opened.
  - `selected` takes the currently selected entity, if any.
  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".
  - `optionDescriptionFn` takes a function that expects an instance of the data being selected from and returns a string describing the instance, i.e. if the label is "Garden Hose", the description may be "30 ft".
  - `optionsContainerMaxHeight` takes a float that specifies the max height of the container of the selectable options.
  - `spinnerColor` takes a `Color` for the loading spinner.
  - `selectTitle` takes a string to label the select in its closed state and non-selected state.
  - `searchPrompt` takes a string to indicate what is being searched for.
  - `characterThresholdPrompt` takes a function that expects an int and returns a string indicating how many more characters need to be entered to trigger the query.
  - `queryErrorMsg` takes a string to indicate that an error has occured while querying data.
  - `noResultsForMsg` takes a function that expects a string and returns a message indicating that the search for the provided string returned no results.
  - `noOptionsMsg` takes a string to indicate that no options exist in the select.

```elm
import SingleSelectRemote
import Html exposing (Html)
import Color

type Msg
    = HandleSelectUpdate (SingleSelectRemote.Msg Product)
    | HandleSelection ( Product, SingleSelectRemote.Msg Product )

type alias Product =
    { name : String
    , description : String
    , price : Float
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { products = exampleProducts
      , select =
            SingleSelectRemote.init
                { selectionMsg = HandleSelection
                , internalMsg = HandleSelectUpdate
                ...
                }
      , selectedProduct = Nothing
      }
    , Cmd.none
    )

type alias Model =
    { products : List Product
    , select : SingleSelectRemote.SmartSelect Msg Product
    , selectedProduct : Maybe Product
    }

viewCustomProductSelect : Model -> Html Msg
viewCustomProductSelect model =
    SingleSelectRemote.viewCustom
        { isDisabled = False
        , selected = model.selectedProduct
        , optionLabelFn = .name
        , optionDescriptionFn = \option -> "$" ++ String.fromFloat option.price
        , optionsContainerMaxHeight = 500
        , spinnerColor = Color.rgb255 0 0 0
        , selectTitle = "Select a Product"
        , searchPrompt = "Search for a Product"
        , characterThresholdPrompt =
            \difference ->
                if difference > 1 then
                    "Please enter " ++ String.fromInt difference ++ " more characters to search for a Product"

                else if difference == 1 then
                    "Please enter 1 more character to search for a Product"

                else
                    ""
        , queryErrorMsg = "An error occured while querying Products"
        , noResultsForMsg = \searchText -> "No results found for: " ++ searchText
        , noOptionsMsg = "There are no options to select"
        }
        model.select
```

-}
handleOnInput : (( Maybe a, Msg a ) -> msg) -> (Msg a -> msg) -> String -> msg
handleOnInput selectionMsg internalMsg value =
    if value == "" then
        selectionMsg ( Nothing, Clear )

    else
        internalMsg <| SetSearchText value


viewCustom :
    { isDisabled : Bool
    , selected : Maybe a
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    , spinnerColor : Color.Color
    , selectTitle : String
    , searchPrompt : String
    , characterThresholdPrompt : Int -> String
    , queryErrorMsg : String
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    }
    -> SmartSelect msg a
    -> Html msg
viewCustom { isDisabled, selected, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight, spinnerColor, selectTitle, searchPrompt, characterThresholdPrompt, queryErrorMsg, noResultsForMsg, noOptionsMsg } (SmartSelect model) =
    let
        selectedLabel =
            Maybe.map (\s -> optionLabelFn s) selected |> Maybe.withDefault ""

        inputValue =
            case ( selected, model.isOpen ) of
                ( Just value, False ) ->
                    optionLabelFn value

                ( _, _ ) ->
                    model.searchText
    in
    viewTextFieldContainer
        [ id (Id.select model.idPrefix)
        , classList
            [ ( classPrefix "enabled-closed", not model.isOpen )
            , ( classPrefix "enabled-opened", model.isOpen )
            ]
        , Events.stopPropagationOn "keypress" (Decode.map Utilities.alwaysStopPropogation (Decode.succeed <| model.internalMsg NoOp))
        , Events.preventDefaultOn "keydown"
            (keyActionMapper
                { remoteData = model.remoteData
                , focusedOptionIndex = model.focusedOptionIndex
                , selectionMsg = model.selectionMsg
                , internalMsg = model.internalMsg
                }
            )
        ]
        [ viewTextField [ onClick <| model.internalMsg <| Open ]
            { inputAttributes =
                [ id (Id.input model.idPrefix)
                , autocomplete False
                , onInput <| handleOnInput model.selectionMsg model.internalMsg
                , placeholder <| searchPrompt
                , value inputValue
                ]
            , isDisabled = isDisabled
            , selectedOptions = []
            , clearIconAttributes = [ Events.stopPropagationOn "click" (Decode.succeed ( model.selectionMsg ( Nothing, Clear ), True )) ]
            }
        , Alignment.view
            model.idPrefix
            model.alignment
            [ viewRemoteData
                { selectionMsg = model.selectionMsg
                , internalMsg = model.internalMsg
                , focusedOptionIndex = model.focusedOptionIndex
                , characterSearchThreshold = model.characterSearchThreshold
                , searchText = model.searchText
                , selectedOption = selected
                , remoteData = model.remoteData
                , optionLabelFn = optionLabelFn
                , optionDescriptionFn = optionDescriptionFn
                , optionsContainerMaxHeight = optionsContainerMaxHeight
                , spinner = model.spinner
                , spinnerColor = spinnerColor
                , characterThresholdPrompt = characterThresholdPrompt
                , queryErrorMsg = queryErrorMsg
                , noResultsForMsg = noResultsForMsg
                , noOptionsMsg = noOptionsMsg
                , idPrefix = model.idPrefix
                }
            ]
        ]
