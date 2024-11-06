module SingleSelectRemote exposing (SmartSelect, Msg, init, view, viewCustom, subscriptions, update, updatePosition)

{-| A select component for a single selection with remote data.


# Architecture

@docs SmartSelect, Msg, init, view, viewCustom, subscriptions, update, updatePosition

-}

import Browser.Dom as Dom
import Browser.Events
import Debounce exposing (Debounce)
import Dict
import Html exposing (Html)
import Html.Styled exposing (text)
import Html.Styled.Attributes exposing (autocomplete, classList, id, placeholder, style, value)
import Html.Styled.Events as Events exposing (onClick, onInput, onMouseEnter)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..))
import SmartSelect.Alignment as Alignment exposing (Alignment)
import SmartSelect.Errors as Errors
import SmartSelect.Id as Id exposing (Prefix(..))
import SmartSelect.Settings exposing (RemoteSettings)
import SmartSelect.Utilities as Utilities exposing (KeyCode(..))
import SmartSelect.ViewComponents exposing (viewEmptyOptionsListItem, viewError, viewOptionsList, viewOptionsListItem, viewSearchPrompt, viewSearchPromptContainer, viewSpinner, viewTextField, viewTextFieldContainer)
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
    , selectionMsg : ( a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , idPrefix : Prefix
    , alignment : Maybe Alignment
    }


{-| The type representing the select's configuration to be passed to SingleSelectRemote.viewCustom
-}
type alias Config msg a =
    { selected : Maybe a
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , remoteSettings : RemoteSettings msg a
    }


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


{-| Instantiates and returns a smart select.

  - `selectionMsg` takes a function that expects a tuple representing the selection and a SinglSelectRemote.Msg msg and returns an externally defined msg for handling selection.
  - `internalMsg` takes a function that expects a SingleSelectRemote.Msg and returns an externally defined msg.
  - `idPrefix` takes a string with a unique prefix

-}
init : { selectionMsg : ( a, Msg a ) -> msg, internalMsg : Msg a -> msg, idPrefix : String } -> SmartSelect msg a
init { selectionMsg, internalMsg, idPrefix } =
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
        , idPrefix = Prefix idPrefix
        , alignment = Nothing
        }


{-| Events external to the smart select to which it is subscribed.
-}
subscriptions : RemoteSettings msg a -> SmartSelect msg a -> Sub msg
subscriptions remoteSettings (SmartSelect model) =
    if model.isOpen then
        Sub.batch
            [ Browser.Events.onResize (\h w -> model.internalMsg <| WindowResized ( h, w ))
            , case model.remoteData of
                NotAsked ->
                    if remoteSettings.characterSearchThreshold == 0 then
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


keyActionMapper : { remoteData : RemoteData ( String, String ) (List a), focusedOptionIndex : Int, selectionMsg : ( a, Msg a ) -> msg, internalMsg : Msg a -> msg } -> Decode.Decoder ( msg, Bool )
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
                                ( selectionMsg ( item, Close ), Utilities.preventDefault key )

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
-}
update : Msg a -> RemoteSettings msg a -> SmartSelect msg a -> ( SmartSelect msg a, Cmd msg )
update msg remoteSettings (SmartSelect model) =
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
            if String.length text < remoteSettings.characterSearchThreshold then
                ( SmartSelect { model | searchText = text, remoteData = NotAsked }, Cmd.none )

            else
                let
                    ( debounce, cmd ) =
                        Debounce.push (debounceConfig { internalMsg = model.internalMsg, debounceDuration = remoteSettings.debounceDuration }) text model.debounce
                in
                ( SmartSelect { model | searchText = text, debounce = debounce }
                , cmd
                )

        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        (debounceConfig { internalMsg = model.internalMsg, debounceDuration = remoteSettings.debounceDuration })
                        (Debounce.takeLast (Utilities.search { remoteQueryAttrs = remoteSettings.queryAttrs, handleResponse = \remoteData -> model.internalMsg <| GotRemoteData remoteData }))
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
                    if remoteSettings.characterSearchThreshold == 0 then
                        Utilities.search { remoteQueryAttrs = remoteSettings.queryAttrs, handleResponse = \remoteData -> model.internalMsg <| GotRemoteData remoteData } ""

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


openPopover : SmartSelect msg a -> ( SmartSelect msg a, Cmd msg )
openPopover (SmartSelect model) =
    ( SmartSelect { model | isOpen = True, searchText = "", focusedOptionIndex = 0 }
    , Cmd.batch
        [ Alignment.getAlignment model.idPrefix (\alignment -> model.internalMsg (GotAlignment alignment))
        , Utilities.focusInput model.idPrefix (model.internalMsg NoOp)
        ]
    )


{-| Triggers an update of the provided smart select's alignment
-}
updatePosition : SmartSelect msg a -> ( SmartSelect msg a, Cmd msg )
updatePosition (SmartSelect model) =
    let
        cmd =
            if model.isOpen then
                Alignment.getAlignment model.idPrefix (\alignment -> model.internalMsg (GotAlignment alignment))

            else
                Cmd.none
    in
    ( SmartSelect model
    , cmd
    )


showOptions :
    { selectionMsg : ( a, Msg a ) -> msg
    , selectedOption : Maybe a
    , internalMsg : Msg a -> msg
    , focusedOptionIndex : Int
    , searchText : String
    , options : List ( Int, a )
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , idPrefix : Prefix
    , remoteSettings : RemoteSettings msg a
    }
    -> Html.Styled.Html msg
showOptions { selectionMsg, selectedOption, internalMsg, focusedOptionIndex, searchText, options, optionLabelFn, optionDescriptionFn, idPrefix, remoteSettings } =
    let
        settings =
            remoteSettings.settings
    in
    viewOptionsList settings.theme
        [ id (Id.container idPrefix)
        , style "max-height" (String.fromFloat settings.optionsContainerMaxHeight ++ "px")
        ]
        (if List.isEmpty options && searchText /= "" then
            [ viewEmptyOptionsListItem settings.theme [] [ text <| settings.noResultsForMsg searchText ] ]

         else if List.isEmpty options then
            [ viewEmptyOptionsListItem settings.theme [] [ text settings.noOptionsMsg ] ]

         else
            List.map
                (\( idx, option ) ->
                    let
                        isSelected =
                            Maybe.map (\value -> value == option) selectedOption
                                |> Maybe.withDefault False
                    in
                    viewOptionsListItem settings.theme
                        [ Events.stopPropagationOn "click" (Decode.succeed ( selectionMsg ( option, Close ), True ))
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
    { selectionMsg : ( a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , focusedOptionIndex : Int
    , searchText : String
    , selectedOption : Maybe a
    , remoteData : RemoteData ( String, String ) (List a)
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , spinner : Spinner.Model
    , idPrefix : Prefix
    , remoteSettings : RemoteSettings msg a
    }
    -> Html.Styled.Html msg
viewRemoteData { selectionMsg, internalMsg, focusedOptionIndex, searchText, selectedOption, remoteData, optionLabelFn, optionDescriptionFn, spinner, idPrefix, remoteSettings } =
    let
        settings =
            remoteSettings.settings
    in
    case remoteData of
        NotAsked ->
            if remoteSettings.characterSearchThreshold == 0 then
                viewSpinner settings.theme { spinner = spinner, spinnerColor = remoteSettings.spinnerColor }

            else
                let
                    difference =
                        remoteSettings.characterSearchThreshold - String.length searchText

                    searchPrompt =
                        if difference == 0 then
                            viewSpinner settings.theme { spinner = spinner, spinnerColor = remoteSettings.spinnerColor }

                        else
                            viewSearchPrompt settings.theme [] [ text <| remoteSettings.characterThresholdPrompt difference ]
                in
                viewSearchPromptContainer settings.theme [] [ searchPrompt ]

        Loading ->
            viewSpinner settings.theme { spinner = spinner, spinnerColor = remoteSettings.spinnerColor }

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
                , idPrefix = idPrefix
                , remoteSettings = remoteSettings
                }

        Failure _ ->
            viewError settings.theme
                []
                { message = remoteSettings.queryErrorMsg, onDismiss = internalMsg DismissError }


indexOptions : List a -> List ( Int, a )
indexOptions options =
    List.indexedMap Tuple.pair options


{-| The smart select view for selecting one option at a time with remote data.

  - `selected` takes the currently selected entity, if any.
  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".
  - `remoteSettings` takes the remoteSettings record (see the `RemoteSettings` type)

-}
view :
    { selected : Maybe a
    , optionLabelFn : a -> String
    , remoteSettings : RemoteSettings msg a
    }
    -> SmartSelect msg a
    -> Html msg
view config smartSelect =
    viewStyled config smartSelect
        |> Html.Styled.toUnstyled


viewStyled :
    { selected : Maybe a
    , optionLabelFn : a -> String
    , remoteSettings : RemoteSettings msg a
    }
    -> SmartSelect msg a
    -> Html.Styled.Html msg
viewStyled { selected, optionLabelFn, remoteSettings } smartSelect =
    let
        config =
            { selected = selected
            , optionLabelFn = optionLabelFn
            , optionDescriptionFn = \_ -> ""
            , remoteSettings = remoteSettings
            }
    in
    viewCustomStyled config smartSelect


{-| The smart select view for selecting one option at a time with remote data. You have to pass a custom configuration here.
-}
viewCustom : Config msg a -> SmartSelect msg a -> Html msg
viewCustom config smartSelect =
    viewCustomStyled config smartSelect
        |> Html.Styled.toUnstyled


viewCustomStyled : Config msg a -> SmartSelect msg a -> Html.Styled.Html msg
viewCustomStyled config (SmartSelect model) =
    let
        { selected, optionLabelFn, optionDescriptionFn, remoteSettings } =
            config

        settings =
            remoteSettings.settings

        inputValue =
            case ( selected, model.isOpen ) of
                ( Just value, False ) ->
                    optionLabelFn value

                ( _, _ ) ->
                    model.searchText
    in
    viewTextFieldContainer settings.theme
        [ id (Id.select model.idPrefix)
        , classList
            [ ( "enabled-closed", not model.isOpen )
            , ( "enabled-opened", model.isOpen )
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
        [ viewTextField settings.theme
            [ onClick <| model.internalMsg <| Open ]
            { inputAttributes =
                [ id (Id.input model.idPrefix)
                , autocomplete False
                , onInput <| \newValue -> model.internalMsg <| SetSearchText newValue
                , placeholder <| settings.placeholder
                , value inputValue
                ]
            , isDisabled = settings.isDisabled
            , selectedOptions = []
            , clearIconAttributes = Nothing
            , icon = settings.icon
            }
        , Alignment.view
            { theme = settings.theme
            , scrollBehavior = settings.scrollBehavior
            , idPrefix = model.idPrefix
            , onClose = model.internalMsg <| Close
            , alignment = model.alignment
            }
            [ viewRemoteData
                { selectionMsg = model.selectionMsg
                , internalMsg = model.internalMsg
                , focusedOptionIndex = model.focusedOptionIndex
                , searchText = model.searchText
                , selectedOption = selected
                , remoteData = model.remoteData
                , optionLabelFn = optionLabelFn
                , optionDescriptionFn = optionDescriptionFn
                , spinner = model.spinner
                , idPrefix = model.idPrefix
                , remoteSettings = remoteSettings
                }
            ]
        ]
