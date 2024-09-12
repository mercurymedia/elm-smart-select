module MultiSelectRemote exposing (SmartSelect, Msg, init, view, viewCustom, subscriptions, update, updatePosition)

{-| A select component for multi selection with remote data.


# Architecture

@docs SmartSelect, Msg, init, view, viewCustom, subscriptions, update, updatePosition

-}

import Browser.Dom as Dom
import Browser.Events
import Debounce exposing (Debounce)
import Dict
import Html exposing (Html)
import Html.Styled exposing (div, text)
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
    , selectionMsg : ( List a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , idPrefix : Prefix
    , alignment : Maybe Alignment
    }


{-| The type representing the select's configuration to be passed to MultiSelectRemote.viewCustom
-}
type alias Config msg a =
    { selected : List a
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , viewSelectedOptionFn : a -> Html.Html msg
    , remoteSettings : RemoteSettings msg a
    }


{-| Opaque type representing cases to be passed to MultiSelectRemote.update
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
    | GotAlignment (Result Dom.Error Alignment)
    | DismissError
    | SelectionChanged (Maybe Int)
    | Open
    | Close
    | Clear


{-| Instantiates and returns a smart select.

  - `selectionMsg` takes a function that expects a tuple representing the list of selections and a MultiSelectRemote.Msg and returns an externally defined msg for handling selection.
  - `internalMsg` takes a function that expects a MultiSelectRemote.Msg and returns an externally defined msg.
  - `idPrefix` takes a string with a unique prefix

-}
init : { selectionMsg : ( List a, Msg a ) -> msg, internalMsg : Msg a -> msg, idPrefix : String } -> SmartSelect msg a
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


keyActionMapper : { remoteData : RemoteData ( String, String ) (List a), selectedOptions : List a, focusedOptionIndex : Int, selectionMsg : ( List a, Msg a ) -> msg, internalMsg : Msg a -> msg, searchText : String } -> Decode.Decoder ( msg, Bool )
keyActionMapper { remoteData, selectedOptions, focusedOptionIndex, selectionMsg, internalMsg, searchText } =
    let
        options =
            case remoteData of
                Success opts ->
                    filterAndIndexOptions { allOptions = opts, selectedOptions = selectedOptions }

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
                                ( selectionMsg ( item :: selectedOptions, SelectionChanged <| Just (Utilities.newFocusedOptionIndexAfterSelection focusedOptionIndex) ), Utilities.preventDefault key )

                            Nothing ->
                                ( internalMsg NoOp, Utilities.preventDefault key )

                    Escape ->
                        ( internalMsg Close, Utilities.preventDefault key )

                    Backspace ->
                        if searchText == "" && List.length selectedOptions > 0 then
                            let
                                newSelectedOptions =
                                    List.take (List.length selectedOptions - 1) selectedOptions
                            in
                            ( selectionMsg ( newSelectedOptions, SelectionChanged <| Just (Utilities.newFocusedOptionIndexAfterSelection focusedOptionIndex) ), Utilities.preventDefault key )

                        else
                            ( internalMsg NoOp, Utilities.preventDefault key )

                    Other ->
                        ( internalMsg NoOp, Utilities.preventDefault key )
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
            ( SmartSelect { model | focusedOptionIndex = idx }, Utilities.focusInput model.idPrefix (model.internalMsg NoOp) )

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
                Ok aligment ->
                    ( SmartSelect { model | alignment = Just aligment }, Cmd.none )

                Err _ ->
                    ( SmartSelect model, Cmd.none )

        SelectionChanged newFocusedOptionIndex ->
            let
                focusedOptionIndex =
                    Maybe.withDefault model.focusedOptionIndex newFocusedOptionIndex
            in
            ( SmartSelect { model | isOpen = True, searchText = "", focusedOptionIndex = focusedOptionIndex }
            , Cmd.batch
                [ Alignment.getAlignment model.idPrefix (\alignment -> model.internalMsg (GotAlignment alignment))
                , Utilities.focusInput model.idPrefix (model.internalMsg NoOp)
                ]
            )

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
                    openPopover (SmartSelect model) ""
            in
            ( updatedModel, Cmd.batch [ popoverCmd, cmd ] )

        Close ->
            ( SmartSelect { model | isOpen = False, searchText = "", alignment = Nothing, remoteData = NotAsked }
            , Utilities.blurInput model.idPrefix (model.internalMsg NoOp)
            )

        Clear ->
            let
                cmd =
                    if model.isOpen then
                        Cmd.batch
                            [ Alignment.getAlignment model.idPrefix (\alignment -> model.internalMsg (GotAlignment alignment))
                            , Utilities.focusInput model.idPrefix (model.internalMsg NoOp)
                            ]

                    else
                        Cmd.none
            in
            ( SmartSelect { model | remoteData = NotAsked, searchText = "" }, cmd )


openPopover : SmartSelect msg a -> String -> ( SmartSelect msg a, Cmd msg )
openPopover (SmartSelect model) searchText =
    ( SmartSelect { model | isOpen = True, searchText = searchText, focusedOptionIndex = 0 }
    , Cmd.batch
        [ Alignment.getAlignment model.idPrefix (\alignment -> model.internalMsg (GotAlignment alignment))
        , Utilities.focusInput model.idPrefix (model.internalMsg NoOp)
        ]
    )


showOptions :
    { selectionMsg : ( List a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , focusedOptionIndex : Int
    , searchText : String
    , selectedOptions : List a
    , options : List ( Int, a )
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , idPrefix : Prefix
    , remoteSettings : RemoteSettings msg a
    }
    -> Html.Styled.Html msg
showOptions { selectionMsg, internalMsg, focusedOptionIndex, searchText, selectedOptions, options, optionLabelFn, optionDescriptionFn, idPrefix, remoteSettings } =
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
                    viewOptionsListItem settings.theme
                        [ Events.stopPropagationOn "click" (Decode.succeed ( selectionMsg ( option :: selectedOptions, SelectionChanged <| Just (Utilities.newFocusedOptionIndexAfterSelection focusedOptionIndex) ), True ))
                        , onMouseEnter <| internalMsg <| SetFocused idx
                        , id <| Id.option idPrefix idx
                        ]
                        { label = optionLabelFn option
                        , description = optionDescriptionFn option
                        , isFocused = idx == focusedOptionIndex
                        , isSelected = False
                        }
                )
                options
        )


viewRemoteData :
    { selectionMsg : ( List a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , focusedOptionIndex : Int
    , searchText : String
    , selectedOptions : List a
    , remoteData : RemoteData ( String, String ) (List a)
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , spinner : Spinner.Model
    , idPrefix : Prefix
    , remoteSettings : RemoteSettings msg a
    }
    -> Html.Styled.Html msg
viewRemoteData { selectionMsg, internalMsg, focusedOptionIndex, searchText, selectedOptions, remoteData, optionLabelFn, optionDescriptionFn, spinner, idPrefix, remoteSettings } =
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
                , internalMsg = internalMsg
                , focusedOptionIndex = focusedOptionIndex
                , searchText = searchText
                , selectedOptions = selectedOptions
                , options = filterAndIndexOptions { allOptions = options, selectedOptions = selectedOptions }
                , optionLabelFn = optionLabelFn
                , optionDescriptionFn = optionDescriptionFn
                , idPrefix = idPrefix
                , remoteSettings = remoteSettings
                }

        Failure _ ->
            viewError settings.theme
                []
                { message = remoteSettings.queryErrorMsg, onDismiss = internalMsg DismissError }


removeSelectedFromOptions : List a -> List a -> List a
removeSelectedFromOptions selectedOptions options =
    List.filter (\el -> not <| List.member el selectedOptions) options


filterAndIndexOptions : { allOptions : List a, selectedOptions : List a } -> List ( Int, a )
filterAndIndexOptions { allOptions, selectedOptions } =
    removeSelectedFromOptions selectedOptions allOptions
        |> List.indexedMap Tuple.pair


selectedEntityWrapper :
    { selectionMsg : ( List a, Msg a ) -> msg
    , viewSelectedOptionFn : a -> Html msg
    , selectedOptions : List a
    }
    -> a
    -> Html.Styled.Html msg
selectedEntityWrapper { selectionMsg, viewSelectedOptionFn, selectedOptions } selectedOption =
    div
        [ Events.stopPropagationOn "click" (Decode.succeed ( selectionMsg ( List.filter (\e -> e /= selectedOption) selectedOptions, SelectionChanged Nothing ), True )) ]
        [ viewSelectedOptionFn selectedOption
            |> Html.Styled.fromUnstyled
        ]


{-| The smart select view for selecting multiple options at a time with remote data.

  - `selected` takes a list of the currently selected entities.
  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".
  - `viewSelectedOptionFn` takes a function that expects and instance of the data being selected from and returns html to render a selected option.
  - `remoteSettings` takes the remoteSettings record (see the `RemoteSettings` type)

-}
view :
    { selected : List a
    , optionLabelFn : a -> String
    , viewSelectedOptionFn : a -> Html msg
    , remoteSettings : RemoteSettings msg a
    }
    -> SmartSelect msg a
    -> Html msg
view config smartSelect =
    viewStyled config smartSelect
        |> Html.Styled.toUnstyled


viewStyled :
    { selected : List a
    , optionLabelFn : a -> String
    , viewSelectedOptionFn : a -> Html msg
    , remoteSettings : RemoteSettings msg a
    }
    -> SmartSelect msg a
    -> Html.Styled.Html msg
viewStyled { selected, optionLabelFn, viewSelectedOptionFn, remoteSettings } smartSelect =
    let
        config =
            { selected = selected
            , optionLabelFn = optionLabelFn
            , optionDescriptionFn = \_ -> ""
            , viewSelectedOptionFn = viewSelectedOptionFn
            , remoteSettings = remoteSettings
            }
    in
    viewCustomStyled config smartSelect


{-| The smart select custom view for selecting multiple options at a time with remote data. You have to pass a custom configuration here.
-}
viewCustom : Config msg a -> SmartSelect msg a -> Html msg
viewCustom config smartSelect =
    viewCustomStyled config smartSelect
        |> Html.Styled.toUnstyled


viewCustomStyled : Config msg a -> SmartSelect msg a -> Html.Styled.Html msg
viewCustomStyled config (SmartSelect model) =
    let
        { selected, optionLabelFn, optionDescriptionFn, viewSelectedOptionFn, remoteSettings } =
            config

        settings =
            remoteSettings.settings
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
                , selectedOptions = selected
                , focusedOptionIndex = model.focusedOptionIndex
                , selectionMsg = model.selectionMsg
                , internalMsg = model.internalMsg
                , searchText = model.searchText
                }
            )
        ]
        [ viewTextField settings.theme
            [ onClick <| model.internalMsg Open ]
            { inputAttributes =
                [ id (Id.input model.idPrefix)
                , autocomplete False
                , onInput <| \val -> model.internalMsg <| SetSearchText val
                , value model.searchText
                , placeholder settings.placeholder
                ]
            , isDisabled = settings.isDisabled
            , selectedOptions =
                List.map
                    (selectedEntityWrapper
                        { selectionMsg = model.selectionMsg
                        , viewSelectedOptionFn = viewSelectedOptionFn
                        , selectedOptions = selected
                        }
                    )
                    selected
            , clearIconAttributes = Just [ Events.stopPropagationOn "click" (Decode.succeed ( model.selectionMsg ( [], Clear ), True )) ]
            , icon = settings.icon
            }
        , Alignment.view
            settings.theme
            model.idPrefix
            model.alignment
            [ viewRemoteData
                { selectionMsg = model.selectionMsg
                , internalMsg = model.internalMsg
                , focusedOptionIndex = model.focusedOptionIndex
                , searchText = model.searchText
                , selectedOptions = selected
                , remoteData = model.remoteData
                , optionLabelFn = optionLabelFn
                , optionDescriptionFn = optionDescriptionFn
                , spinner = model.spinner
                , idPrefix = model.idPrefix
                , remoteSettings = remoteSettings
                }
            ]
        ]
