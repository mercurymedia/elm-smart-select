module SingleSelectRemote exposing
    ( SmartSelect, Msg, init, view, viewCustom, subscriptions, update
    , selected
    , setSelected
    )

{-| A select component for a single selection with remote data.


# Architecture

@docs SmartSelect, Msg, init, view, viewCustom, subscriptions, update


# Query

@docs selected


# Externally Triggered Actions

@docs setSelected

-}

import Browser.Dom as Dom exposing (Element)
import Browser.Events
import Color
import Debounce exposing (Debounce)
import Dict
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (autocomplete, class, classList, id, style)
import Html.Events as Events exposing (onClick, onInput, onMouseEnter)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..))
import SmartSelect.Errors as Errors
import SmartSelect.Icons as Icons
import SmartSelect.Utilities as Utilities exposing (KeyCode(..), RemoteQueryAttrs)
import Spinner
import Task


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
    , selected : Maybe a
    , remoteData : RemoteData ( String, String ) (List a)
    , focusedIndex : Int
    , internalMsg : Msg a -> msg
    , characterSearchThreshold : Int
    , debounceDuration : Float
    }


{-| Opaque type representing cases to be passed to SmartSelect.update
-}
type Msg a
    = NoOp
    | SetFocused Int
    | HandleSelection a
    | UpKeyPressed Int
    | DownKeyPressed Int
    | SetSearchText String
    | DebounceMsg Debounce.Msg
    | SpinnerMsg Spinner.Msg
    | GotRemoteData (RemoteData Http.Error (List a))
    | WindowResized ( Int, Int )
    | MaybeGotSelect (Result Dom.Error Element)
    | DismissError
    | Open
    | Close


{-| Instantiates and returns a smart select.

  - `internalMsg` takes a function that expects a SmartSelect.Msg and returns an externally defined msg.
  - `characterSearchThreshold` takes an integer that specifies how many characters need to be typed before triggering the remote query.
  - `debounceDuration` takes a float that specifies the duration in milliseconds between the last keypress and remote query being triggered.

-}
init : { internalMsg : Msg a -> msg, characterSearchThreshold : Int, debounceDuration : Float } -> SmartSelect msg a
init { internalMsg, characterSearchThreshold, debounceDuration } =
    SmartSelect
        { selectWidth = 0
        , isOpen = False
        , searchText = ""
        , debounce = Debounce.init
        , spinner = Spinner.init
        , selected = Nothing
        , remoteData = NotAsked
        , focusedIndex = 0
        , internalMsg = internalMsg
        , characterSearchThreshold = characterSearchThreshold
        , debounceDuration = debounceDuration
        }


smartSelectId : String
smartSelectId =
    "smart-select-component"


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
            , Browser.Events.onMouseDown (clickedOutsideSelect smartSelectId model.internalMsg)
            ]

    else
        Sub.none


clickedOutsideSelect : String -> (Msg a -> msg) -> Decode.Decoder msg
clickedOutsideSelect componentId internalMsg =
    Decode.field "target" (Utilities.eventIsOutsideComponent componentId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| internalMsg Close

                else
                    Decode.fail "inside component"
            )


keyActionMapper : { remoteData : RemoteData ( String, String ) (List a), selectedOption : Maybe a, focusedIndex : Int, internalMsg : Msg a -> msg } -> Decode.Decoder ( msg, Bool )
keyActionMapper { remoteData, selectedOption, focusedIndex, internalMsg } =
    let
        filteredOptions =
            case remoteData of
                Success opts ->
                    filterAndIndexOptions { allOptions = opts, selectedOption = selectedOption }

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
                                if focusedIndex - 1 < 0 then
                                    0

                                else
                                    focusedIndex - 1
                        in
                        ( internalMsg <| UpKeyPressed newIdx, Utilities.preventDefault key )

                    Down ->
                        let
                            newIdx =
                                if focusedIndex + 1 > (List.length filteredOptions - 1) then
                                    List.length filteredOptions - 1

                                else
                                    focusedIndex + 1
                        in
                        ( internalMsg <| DownKeyPressed newIdx, Utilities.preventDefault key )

                    Enter ->
                        case Dict.get focusedIndex (Dict.fromList filteredOptions) of
                            Just item ->
                                ( internalMsg <| HandleSelection item, Utilities.preventDefault key )

                            Nothing ->
                                ( internalMsg NoOp, Utilities.preventDefault key )

                    Escape ->
                        ( internalMsg Close, Utilities.preventDefault key )

                    Other ->
                        ( internalMsg NoOp, Utilities.preventDefault key )
            )


debounceConfig : { internalMsg : Msg a -> msg, debounceDuration : Float } -> Debounce.Config msg
debounceConfig { internalMsg, debounceDuration } =
    { strategy = Debounce.later debounceDuration
    , transform = \debounceMsg -> internalMsg <| DebounceMsg debounceMsg
    }


{-| Get the currently selected entity if any.
-}
selected : SmartSelect msg a -> Maybe a
selected (SmartSelect model) =
    model.selected


{-| It is possible that the select is instantiated on your model before data representing
a previous selection is loaded. Use this function to update the picked selection in
the select when the appropriate data is received.
-}
setSelected : Maybe a -> SmartSelect msg a -> SmartSelect msg a
setSelected newSelected (SmartSelect model) =
    SmartSelect { model | selected = newSelected }


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
            ( SmartSelect { model | focusedIndex = idx }, Cmd.none )

        HandleSelection newSelected ->
            ( SmartSelect { model | isOpen = False, searchText = "", selected = Just newSelected, remoteData = NotAsked }, Cmd.none )

        UpKeyPressed idx ->
            ( SmartSelect { model | focusedIndex = idx }, scrollToOption model.internalMsg idx )

        DownKeyPressed idx ->
            ( SmartSelect { model | focusedIndex = idx }, scrollToOption model.internalMsg idx )

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
                        (Debounce.takeLast (search { remoteQueryAttrs = remoteQueryAttrs, internalMsg = model.internalMsg }))
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
            ( SmartSelect { model | focusedIndex = 0, remoteData = RemoteData.mapError (Errors.httpErrorToReqErrTuple "GET") data }, Cmd.none )

        WindowResized _ ->
            ( SmartSelect model, getSelectWidth model.internalMsg )

        MaybeGotSelect result ->
            case result of
                Ok component ->
                    let
                        selectWidth =
                            component.element |> (\el -> el.width)
                    in
                    ( SmartSelect { model | selectWidth = selectWidth }, focusInput model.internalMsg )

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
                        Cmd.batch [ search { remoteQueryAttrs = remoteQueryAttrs, internalMsg = model.internalMsg } "", getSelectWidth model.internalMsg ]

                    else
                        Cmd.batch [ getSelectWidth model.internalMsg, focusInput model.internalMsg ]
            in
            ( SmartSelect { model | isOpen = True, focusedIndex = 0 }, cmd )

        Close ->
            ( SmartSelect { model | isOpen = False, searchText = "", remoteData = NotAsked }, Cmd.none )


search : { remoteQueryAttrs : RemoteQueryAttrs a, internalMsg : Msg a -> msg } -> String -> Cmd msg
search { remoteQueryAttrs, internalMsg } searchText =
    Http.request
        { method = "GET"
        , headers = remoteQueryAttrs.headers
        , url = remoteQueryAttrs.url searchText
        , body = Http.emptyBody
        , expect = Http.expectJson (\results -> RemoteData.fromResult results |> (\remoteData -> internalMsg <| GotRemoteData remoteData)) (Utilities.decodeOptions remoteQueryAttrs.optionDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


focusInput : (Msg a -> msg) -> Cmd msg
focusInput internalMsg =
    Task.attempt (\_ -> internalMsg NoOp) (Dom.focus "smart-select-input")


getSelectWidth : (Msg a -> msg) -> Cmd msg
getSelectWidth internalMsg =
    Task.attempt (\select -> internalMsg <| MaybeGotSelect select) (Dom.getElement "smart-select-component")


scrollToOption : (Msg a -> msg) -> Int -> Cmd msg
scrollToOption internalMsg idx =
    Task.attempt (\_ -> internalMsg NoOp) (scrollTask idx)


scrollTask : Int -> Task.Task Dom.Error ()
scrollTask idx =
    Task.sequence
        [ Dom.getElement (optionId idx) |> Task.map (\x -> x.element.y)
        , Dom.getElement (optionId idx) |> Task.map (\x -> x.element.height)
        , Dom.getElement "elm-smart-select--select-options-container" |> Task.map (\x -> x.element.y)
        , Dom.getElement "elm-smart-select--select-options-container" |> Task.map (\x -> x.element.height)
        , Dom.getViewportOf "elm-smart-select--select-options-container" |> Task.map (\x -> x.viewport.y)
        ]
        |> Task.andThen
            (\outcome ->
                case outcome of
                    optionY :: optionHeight :: containerY :: containerHeight :: containerScrollTop :: [] ->
                        if (optionY + optionHeight) >= containerY + containerHeight then
                            Dom.setViewportOf "elm-smart-select--select-options-container" 0 (containerScrollTop + ((optionY - (containerY + containerHeight)) + optionHeight))
                                |> Task.onError (\_ -> Task.succeed ())

                        else if optionY < containerY then
                            Dom.setViewportOf "elm-smart-select--select-options-container" 0 (containerScrollTop + (optionY - containerY))
                                |> Task.onError (\_ -> Task.succeed ())

                        else
                            Task.succeed ()

                    _ ->
                        Task.succeed ()
            )


classPrefix : String
classPrefix =
    "elm-smart-select--"


optionId : Int -> String
optionId idx =
    "option-" ++ String.fromInt idx


showSpinner : { spinner : Spinner.Model, spinnerColor : Color.Color } -> Html msg
showSpinner { spinner, spinnerColor } =
    div [ class (classPrefix ++ "loading-spinner-container") ] [ div [ class (classPrefix ++ "loading-spinner") ] [ Spinner.view (Utilities.spinnerConfig spinnerColor) spinner ] ]


showOptions :
    { internalMsg : Msg a -> msg
    , focusedIndex : Int
    , searchText : String
    , options : List ( Int, a )
    , optionType : String
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    }
    -> Html msg
showOptions { internalMsg, focusedIndex, searchText, options, optionType, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight } =
    if List.isEmpty options && searchText /= "" then
        div [ class (classPrefix ++ "search-or-no-results-text") ] [ text ("No results found for: " ++ searchText) ]

    else if List.isEmpty options then
        div [ class (classPrefix ++ "search-or-no-results-text") ] [ text ("No " ++ optionType ++ "s are available") ]

    else
        div [ class (classPrefix ++ "select-options-container"), style "max-height" (String.fromFloat optionsContainerMaxHeight ++ "px"), style "overflow" "auto" ]
            (List.map
                (\( idx, option ) ->
                    div
                        [ Events.stopPropagationOn "click" (Decode.succeed ( internalMsg <| HandleSelection option, True ))
                        , onMouseEnter <| internalMsg <| SetFocused idx
                        , id <| optionId idx
                        , classList
                            [ ( classPrefix ++ "select-option", True ), ( classPrefix ++ "select-option-focused", idx == focusedIndex ) ]
                        ]
                        [ div [] [ text (optionLabelFn option) ]
                        , div
                            [ classList
                                [ ( classPrefix ++ "select-option-description", True )
                                , ( classPrefix ++ "select-option-description-unfocused", idx /= focusedIndex )
                                , ( classPrefix ++ "select-option-description-focused", idx == focusedIndex )
                                ]
                            ]
                            [ text (optionDescriptionFn option) ]
                        ]
                )
                options
            )


viewRemoteData :
    { internalMsg : Msg a -> msg
    , focusedIndex : Int
    , characterSearchThreshold : Int
    , searchText : String
    , selectedOption : Maybe a
    , remoteData : RemoteData ( String, String ) (List a)
    , optionType : String
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    , spinner : Spinner.Model
    , spinnerColor : Color.Color
    }
    -> Html msg
viewRemoteData { internalMsg, focusedIndex, characterSearchThreshold, searchText, selectedOption, remoteData, optionType, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight, spinner, spinnerColor } =
    case remoteData of
        NotAsked ->
            if characterSearchThreshold == 0 then
                showSpinner { spinner = spinner, spinnerColor = spinnerColor }

            else
                let
                    difference =
                        characterSearchThreshold - String.length searchText

                    searchPrompt =
                        if characterSearchThreshold > 0 && difference == 0 then
                            showSpinner { spinner = spinner, spinnerColor = spinnerColor }

                        else if difference > 1 then
                            div [ class (classPrefix ++ "search-prompt") ] [ text <| "Please enter " ++ String.fromInt difference ++ " more characters to search for a " ++ String.toLower optionType ]

                        else
                            div [ class (classPrefix ++ "search-prompt") ] [ text <| "Please enter 1 more character to search for a " ++ String.toLower optionType ]
                in
                div [ class (classPrefix ++ "search-prompt-container") ] [ searchPrompt ]

        Loading ->
            showSpinner { spinner = spinner, spinnerColor = spinnerColor }

        Success options ->
            showOptions
                { internalMsg = internalMsg
                , focusedIndex = focusedIndex
                , searchText = searchText
                , options = filterAndIndexOptions { allOptions = options, selectedOption = selectedOption }
                , optionType = optionType
                , optionLabelFn = optionLabelFn
                , optionDescriptionFn = optionDescriptionFn
                , optionsContainerMaxHeight = optionsContainerMaxHeight
                }

        Failure ( requestDecorator, errMsg ) ->
            div [ class (classPrefix ++ "error-box-container") ]
                [ div [ class (classPrefix ++ "error-box") ]
                    [ div [ class (classPrefix ++ "error-container") ]
                        [ div [ class (classPrefix ++ "request-decorator") ] [ text requestDecorator ]
                        , div [] [ text errMsg ]
                        ]
                    , span
                        [ class (classPrefix ++ "dismiss-error-x")
                        , onClick <| internalMsg DismissError
                        ]
                        [ Icons.x
                            |> Icons.withSize 12
                            |> Icons.withStrokeWidth 4
                            |> Icons.toHtml []
                        ]
                    ]
                ]


removeSelectedFromOptions : Maybe a -> List a -> List a
removeSelectedFromOptions selectedOption options =
    Maybe.map (\s -> List.filter (\el -> el /= s) options) selectedOption
        |> Maybe.withDefault options


filterAndIndexOptions : { allOptions : List a, selectedOption : Maybe a } -> List ( Int, a )
filterAndIndexOptions { allOptions, selectedOption } =
    removeSelectedFromOptions selectedOption allOptions
        |> List.indexedMap Tuple.pair


{-| The smart select view for selecting one option at a time with remote data. Takes a function
that expects an instance of the data being selected from and returns a string naming/labeling the
instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".
-}
view : (a -> String) -> SmartSelect msg a -> Html msg
view optionLabelFn smartSelect =
    let
        config =
            { isDisabled = False
            , optionType = "Option"
            , optionLabelFn = optionLabelFn
            , optionDescriptionFn = \_ -> ""
            , optionsContainerMaxHeight = 300
            , spinnerColor = Color.rgb255 57 179 181
            }
    in
    viewCustom config smartSelect


{-| The customizable smart select view for selecting one option at a time with remote data. It expects the following arguments (in order):

  - `isDisabled` takes a boolean that indicates whether or not the select can be opened.
  - `optionType` takes a string that indicates what kind of data is being selected, i.e. "Product" or "Client".
  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".
  - `optionDescriptionFn` takes a function that expects an instance of the data being selected from and returns a string describing the instance, i.e. if the label is "Garden Hose", the description may be "30 ft".
  - `optionsContainerMaxHeight` takes a float that specifies the max height of the container of the selectable options.
  - `spinnerColor` takes a `Color` for the loading spinner.

-}
viewCustom :
    { isDisabled : Bool
    , optionType : String
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    , spinnerColor : Color.Color
    }
    -> SmartSelect msg a
    -> Html msg
viewCustom { isDisabled, optionType, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight, spinnerColor } (SmartSelect model) =
    let
        selectedLabel =
            Maybe.map (\s -> optionLabelFn s) model.selected |> Maybe.withDefault optionType
    in
    if isDisabled then
        div
            [ id smartSelectId
            , class (String.join " " [ classPrefix ++ "selector-container", classPrefix ++ "single-bg-color", classPrefix ++ "disabled" ])
            ]
            [ div [ class (classPrefix ++ "label-and-selector-container") ]
                [ div [ class (classPrefix ++ "label") ] [ text selectedLabel ] ]
            ]

    else
        div
            [ id smartSelectId
            , classList
                [ ( String.join " " [ classPrefix ++ "selector-container", classPrefix ++ "single-bg-color" ], True )
                , ( classPrefix ++ "enabled-closed", not model.isOpen )
                , ( classPrefix ++ "enabled-opened", model.isOpen )
                ]
            , onClick <| model.internalMsg Open
            , Events.preventDefaultOn "keydown"
                (keyActionMapper
                    { remoteData = model.remoteData
                    , selectedOption = model.selected
                    , focusedIndex = model.focusedIndex
                    , internalMsg = model.internalMsg
                    }
                )
            ]
            [ div [ class (classPrefix ++ "label-and-selector-container") ]
                [ div [ class (classPrefix ++ "label") ] [ text selectedLabel ]
                , if model.isOpen then
                    -- figure out alignment issue if possible instead of using 'left -1px'
                    div
                        [ style "width" (String.fromFloat model.selectWidth ++ "px")
                        , style "left" "-1px"
                        , classList
                            [ ( String.join " " [ classPrefix ++ "options-container", classPrefix ++ "single-bg-color" ], True )
                            , ( classPrefix ++ "invisible", model.selectWidth == 0 )
                            ]
                        ]
                        [ div
                            [ class (classPrefix ++ "single-selector-input-container") ]
                            [ input [ id "smart-select-input", class (classPrefix ++ "single-selector-input"), autocomplete False, onInput <| \val -> model.internalMsg <| SetSearchText val ] [] ]
                        , viewRemoteData
                            { internalMsg = model.internalMsg
                            , focusedIndex = model.focusedIndex
                            , characterSearchThreshold = model.characterSearchThreshold
                            , searchText = model.searchText
                            , selectedOption = model.selected
                            , remoteData = model.remoteData
                            , optionType = optionType
                            , optionLabelFn = optionLabelFn
                            , optionDescriptionFn = optionDescriptionFn
                            , optionsContainerMaxHeight = optionsContainerMaxHeight
                            , spinner = model.spinner
                            , spinnerColor = spinnerColor
                            }
                        ]

                  else
                    text ""
                ]
            ]
