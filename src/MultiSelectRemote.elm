module MultiSelectRemote exposing
    ( SmartSelect, Msg, init, view, subscriptions, update
    , Settings
    , selected
    , setSelected
    )

{-| A select component for multi selection with remote data.


# Architecture

@docs SmartSelect, Msg, init, view, subscriptions, update


# Settings and Configuration

@docs Settings


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
import SmartSelect.Utilities as Utilities exposing (KeyCode(..), RemoteSearchAttrs)
import Spinner
import Task


{-| The opaque type representing a particular smart select instance.
-}
type SmartSelect msg a
    = SmartSelect (Model msg a)


type alias Model msg a =
    { settings : Settings msg a
    , selectWidth : Float
    , isOpen : Bool
    , searchText : String
    , debounce : Debounce String
    , spinner : Spinner.Model
    , selected : List a
    , remoteData : RemoteData ( String, String ) (List a)
    , focusedIndex : Int
    }


{-| The type facilitating the configuration of the smart select.

  - The `internalMsg` field takes a function that expects a SmartSelect.Msg and returns an externally defined msg.
  - `optionType` is a string that indicates what kind of data is being selected, i.e. "Product" or "Client"
  - `optionLabel` expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose"
  - `optionDescription` expects an instance of the data being selected from and returns a string describing the instance, i.e. if the label is "Garden Hose", the description may be "30 ft"
      - Because the smart select is unaware of the type and structure of the data it is processing, these functions are necessary to help render the options in the select dropdown.
  - The `optionsContainerMaxHeight` and `optionHeight` fields specify the height of the container of the selectable options and the options themselves, respectively. These fields help facilitate scroll functionality.
  - The `searchFn` field expects an instance of `RemoteSearchAttrs`, seen below. This type helps facilitate the remote request for data.
  - `debounceDuration` indicates how long if at all to wait between the last keypress and executing a search. This is particularly useful if the search being executed is pinging an external source.
  - `spinnerColor` indicates the color that the loading spinner should be.
  - `characterThreshold` indicates how many if any characters should be typed before a search is executed.
  - `closeOnSelect` indicates whether or not the `SmartSelect` should close itself after a selection has been made.

```elm
type alias RemoteSearchAttrs a =
    { headers : List Header
    , url : String -> String
    , optionDecoder : Decoder a
    }
```

-}
type alias Settings msg a =
    { internalMsg : Msg a -> msg
    , optionType : String
    , optionLabel : a -> String
    , optionDescription : a -> String
    , optionsContainerMaxHeight : Float
    , optionHeight : Float
    , searchAttrs : RemoteSearchAttrs a
    , spinnerColor : Color.Color
    , debounceDuration : Float
    , characterSearchThreshold : Int
    , closeOnSelect : Bool
    }


{-| Opaque type representing cases to be passed to SmartSelect.update
-}
type Msg a
    = NoOp
    | SetFocused Int
    | HandleSelection ( Int, List a )
    | HandleDeselection (List a)
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


{-| Instantiates and returns a smart select. Takes in the select configuration and a list of previously selected elements, if any.
-}
init : Settings msg a -> List a -> SmartSelect msg a
init settings alreadySelected =
    SmartSelect
        { settings = settings
        , selectWidth = 0
        , isOpen = False
        , searchText = ""
        , debounce = Debounce.init
        , spinner = Spinner.init
        , selected = alreadySelected
        , remoteData = NotAsked
        , focusedIndex = 0
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
            [ Browser.Events.onResize (\h w -> model.settings.internalMsg <| WindowResized ( h, w ))
            , case model.remoteData of
                NotAsked ->
                    if model.settings.characterSearchThreshold == 0 then
                        Sub.map (\sMsg -> model.settings.internalMsg <| SpinnerMsg sMsg) Spinner.subscription

                    else
                        Sub.none

                Loading ->
                    Sub.map (\sMsg -> model.settings.internalMsg <| SpinnerMsg sMsg) Spinner.subscription

                _ ->
                    Sub.none
            , Browser.Events.onMouseDown (clickedOutsideSelect smartSelectId model.settings)
            ]

    else
        Sub.none


clickedOutsideSelect : String -> Settings msg a -> Decode.Decoder msg
clickedOutsideSelect componentId settings =
    Decode.field "target" (Utilities.eventIsOutsideComponent componentId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| settings.internalMsg Close

                else
                    Decode.fail "inside component"
            )


keyActionMapper : Model msg a -> Decode.Decoder ( msg, Bool )
keyActionMapper model =
    let
        options =
            case model.remoteData of
                Success opts ->
                    filterAndIndexOptions model opts

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
                                if model.focusedIndex - 1 < 0 then
                                    0

                                else
                                    model.focusedIndex - 1
                        in
                        ( model.settings.internalMsg <| UpKeyPressed newIdx, Utilities.preventDefault key )

                    Down ->
                        let
                            newIdx =
                                if model.focusedIndex + 1 > (List.length options - 1) then
                                    List.length options - 1

                                else
                                    model.focusedIndex + 1
                        in
                        ( model.settings.internalMsg <| DownKeyPressed newIdx, Utilities.preventDefault key )

                    Enter ->
                        case Dict.get model.focusedIndex (Dict.fromList options) of
                            Just item ->
                                ( model.settings.internalMsg <| HandleSelection <| ( Utilities.newFocusedIndexAfterSelection model.focusedIndex, item :: model.selected ), Utilities.preventDefault key )

                            Nothing ->
                                ( model.settings.internalMsg NoOp, Utilities.preventDefault key )

                    Escape ->
                        ( model.settings.internalMsg Close, Utilities.preventDefault key )

                    Other ->
                        ( model.settings.internalMsg NoOp, Utilities.preventDefault key )
            )


debounceConfig : Settings msg a -> Debounce.Config msg
debounceConfig settings =
    { strategy = Debounce.later settings.debounceDuration
    , transform = \debounceMsg -> settings.internalMsg <| DebounceMsg debounceMsg
    }


{-| Get the currently selected entities if any.
-}
selected : SmartSelect msg a -> List a
selected (SmartSelect model) =
    model.selected


{-| It is possible that the select is instantiated on your model before data representing
a previous selection is loaded. Use this function to update the picked selection in
the select when the appropriate data is received. Use this method sparingly, if at all.
The selected state should ideally only change due to user input.
-}
setSelected : List a -> SmartSelect msg a -> SmartSelect msg a
setSelected newSelected (SmartSelect model) =
    SmartSelect { model | selected = newSelected }


{-| Update the provided smart select and receive the updated select instance and a cmd to run.
-}
update : Msg a -> SmartSelect msg a -> ( SmartSelect msg a, Cmd msg )
update msg (SmartSelect model) =
    case msg of
        NoOp ->
            ( SmartSelect model, Cmd.none )

        SetFocused idx ->
            ( SmartSelect { model | focusedIndex = idx }, Cmd.none )

        HandleSelection ( idx, newSelected ) ->
            if model.settings.closeOnSelect then
                ( SmartSelect { model | isOpen = False, searchText = "", selected = newSelected, remoteData = NotAsked }, Cmd.none )

            else
                ( SmartSelect { model | focusedIndex = idx, selected = newSelected }, focusInput model.settings )

        HandleDeselection newSelected ->
            ( SmartSelect { model | selected = newSelected }, focusInput model.settings )

        UpKeyPressed idx ->
            ( SmartSelect { model | focusedIndex = idx }, scrollToOption model.settings idx )

        DownKeyPressed idx ->
            ( SmartSelect { model | focusedIndex = idx }, scrollToOption model.settings idx )

        SetSearchText text ->
            if String.length text < model.settings.characterSearchThreshold then
                ( SmartSelect { model | searchText = text, remoteData = NotAsked }, Cmd.none )

            else
                let
                    ( debounce, cmd ) =
                        Debounce.push (debounceConfig model.settings) text model.debounce
                in
                ( SmartSelect { model | searchText = text, debounce = debounce }
                , cmd
                )

        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        (debounceConfig model.settings)
                        (Debounce.takeLast (search model.settings))
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
            ( SmartSelect model, getSelectWidth model.settings )

        MaybeGotSelect result ->
            case result of
                Ok component ->
                    let
                        selectWidth =
                            component.element |> (\el -> el.width)
                    in
                    ( SmartSelect { model | selectWidth = selectWidth }, focusInput model.settings )

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
                    if model.settings.characterSearchThreshold == 0 then
                        Cmd.batch [ search model.settings "", getSelectWidth model.settings ]

                    else
                        Cmd.batch [ getSelectWidth model.settings, focusInput model.settings ]
            in
            ( SmartSelect { model | isOpen = True, focusedIndex = 0 }, cmd )

        Close ->
            ( SmartSelect { model | isOpen = False, searchText = "", remoteData = NotAsked }, Cmd.none )


search : Settings msg a -> String -> Cmd msg
search { searchAttrs, internalMsg } searchText =
    Http.request
        { method = "GET"
        , headers = searchAttrs.headers
        , url = searchAttrs.url searchText
        , body = Http.emptyBody
        , expect = Http.expectJson (\results -> RemoteData.fromResult results |> (\remoteData -> internalMsg <| GotRemoteData remoteData)) (Utilities.decodeOptions searchAttrs.optionDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


focusInput : Settings msg a -> Cmd msg
focusInput settings =
    Task.attempt (\_ -> settings.internalMsg NoOp) (Dom.focus "smart-select-input")


getSelectWidth : Settings msg a -> Cmd msg
getSelectWidth settings =
    Task.attempt (\select -> settings.internalMsg <| MaybeGotSelect select) (Dom.getElement "smart-select-component")


scrollToOption : Settings msg a -> Int -> Cmd msg
scrollToOption settings idx =
    Task.attempt (\_ -> settings.internalMsg NoOp) (scrollTask settings idx)


scrollTask : Settings msg a -> Int -> Task.Task Dom.Error ()
scrollTask settings idx =
    Task.sequence
        [ Dom.getElement (optionId idx) |> Task.map (\x -> x.element.y)
        , Dom.getElement "elm-smart-select--select-options-container" |> Task.map (\x -> x.element.y)
        , Dom.getViewportOf "elm-smart-select--select-options-container" |> Task.map (\x -> x.viewport.y)
        ]
        |> Task.andThen
            (\outcome ->
                case outcome of
                    optionY :: containerY :: containerScrollTop :: [] ->
                        if optionY >= containerY + settings.optionsContainerMaxHeight then
                            Dom.setViewportOf "elm-smart-select--select-options-container" 0 (containerScrollTop + settings.optionHeight)
                                |> Task.onError (\_ -> Task.succeed ())

                        else if optionY < containerY then
                            Dom.setViewportOf "elm-smart-select--select-options-container" 0 (containerScrollTop - settings.optionHeight)
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


showSpinner : Model msg a -> Html msg
showSpinner model =
    div [ class (classPrefix ++ "loading-spinner-container") ] [ div [ class (classPrefix ++ "loading-spinner") ] [ Spinner.view (Utilities.spinnerConfig model.settings.spinnerColor) model.spinner ] ]


showOptions : Model msg a -> List ( Int, a ) -> Html msg
showOptions model options =
    if List.isEmpty options && model.searchText /= "" then
        div [ class (classPrefix ++ "search-or-no-results-text") ] [ text ("No results found for: " ++ model.searchText) ]

    else if List.isEmpty options then
        div [ class (classPrefix ++ "search-or-no-results-text") ] [ text ("No " ++ model.settings.optionType ++ "s are available") ]

    else
        div [ class (classPrefix ++ "select-options-container"), style "max-height" (String.fromFloat model.settings.optionsContainerMaxHeight ++ "px"), style "overflow" "auto" ]
            (List.map
                (\( idx, option ) ->
                    div
                        [ Events.stopPropagationOn "click" (Decode.succeed ( model.settings.internalMsg <| HandleSelection ( Utilities.newFocusedIndexAfterSelection model.focusedIndex, option :: model.selected ), True ))
                        , onMouseEnter <| model.settings.internalMsg <| SetFocused idx
                        , id <| optionId idx
                        , classList
                            [ ( classPrefix ++ "select-option", True ), ( classPrefix ++ "select-option-focused", idx == model.focusedIndex ) ]
                        , style "height" (String.fromFloat model.settings.optionHeight ++ "px")
                        ]
                        [ div [] [ text (model.settings.optionLabel option) ]
                        , div
                            [ classList
                                [ ( classPrefix ++ "select-option-description", True )
                                , ( classPrefix ++ "select-option-description-unfocused", idx /= model.focusedIndex )
                                , ( classPrefix ++ "select-option-description-focused", idx == model.focusedIndex )
                                ]
                            ]
                            [ text (model.settings.optionDescription option) ]
                        ]
                )
                options
            )


viewRemoteData : Model msg a -> Html msg
viewRemoteData model =
    case model.remoteData of
        NotAsked ->
            if model.settings.characterSearchThreshold == 0 then
                showSpinner model

            else
                let
                    difference =
                        model.settings.characterSearchThreshold - String.length model.searchText

                    searchPrompt =
                        if model.settings.characterSearchThreshold > 0 && difference == 0 then
                            showSpinner model

                        else if difference > 1 then
                            div [ class (classPrefix ++ "search-prompt") ] [ text <| "Please enter " ++ String.fromInt difference ++ " more characters to search for a " ++ String.toLower model.settings.optionType ]

                        else
                            div [ class (classPrefix ++ "search-prompt") ] [ text <| "Please enter 1 more character to search for a " ++ String.toLower model.settings.optionType ]
                in
                div [ class (classPrefix ++ "search-prompt-container") ] [ searchPrompt ]

        Loading ->
            showSpinner model

        Success options ->
            showOptions model <| filterAndIndexOptions model options

        Failure ( requestDecorator, errMsg ) ->
            div [ class (classPrefix ++ "error-box-container") ]
                [ div [ class (classPrefix ++ "error-box") ]
                    [ div [ class (classPrefix ++ "error-container") ]
                        [ div [ class (classPrefix ++ "request-decorator") ] [ text requestDecorator ]
                        , div [] [ text errMsg ]
                        ]
                    , span
                        [ class (classPrefix ++ "dismiss-error-x")
                        , onClick <| model.settings.internalMsg DismissError
                        ]
                        [ Icons.x
                            |> Icons.withSize 12
                            |> Icons.withStrokeWidth 4
                            |> Icons.toHtml []
                        ]
                    ]
                ]


removeSelectedFromOptions : List a -> List a -> List a
removeSelectedFromOptions selectedOptions options =
    List.filter (\el -> not <| List.member el selectedOptions) options


filterAndIndexOptions : Model msg a -> List a -> List ( Int, a )
filterAndIndexOptions model unfilteredOptions =
    removeSelectedFromOptions model.selected unfilteredOptions
        |> List.indexedMap Tuple.pair


selectedEntityWrapper : Model msg a -> (a -> Html msg) -> a -> Html msg
selectedEntityWrapper model selectedViewFn entity =
    div
        [ class (classPrefix ++ "selected-entity-wrapper"), Events.stopPropagationOn "click" (Decode.succeed ( model.settings.internalMsg <| HandleDeselection <| List.filter (\e -> e /= entity) model.selected, True )) ]
        [ selectedViewFn entity ]


{-| The smart select view for selecting multiple options at a time with remote data. It expects the following arguments (in order):

  - a boolean indicating if the select is disabled or not
  - a function that takes in an instance of the data being selected from and returns html for rendering selected items. This allows the end user to define how they would like to render selected items.
  - the smart select instance

-}
view : Bool -> (a -> Html msg) -> SmartSelect msg a -> Html msg
view isDisabled selectedViewFn (SmartSelect model) =
    if isDisabled then
        div
            [ id smartSelectId
            , class
                (String.join " "
                    [ classPrefix ++ "selector-container"
                    , classPrefix ++ "multi-selector-container-min-height"
                    , classPrefix ++ "multi-bg-color"
                    , classPrefix ++ "disabled"
                    ]
                )
            ]
            []

    else
        div
            [ id smartSelectId
            , onClick <| model.settings.internalMsg Open
            , Events.preventDefaultOn "keydown" (keyActionMapper model)
            , classList
                [ ( String.join " " [ classPrefix ++ "selector-container", classPrefix ++ "multi-selector-container-min-height", classPrefix ++ "multi-bg-color" ], True )
                , ( classPrefix ++ "enabled-closed", not model.isOpen )
                , ( classPrefix ++ "enabled-opened", model.isOpen )
                ]
            ]
            [ div [ class (classPrefix ++ "multi-selected-and-results-container") ]
                [ if model.isOpen then
                    div [ class (classPrefix ++ "multi-selected-container") ]
                        ([ div
                            [ class (classPrefix ++ "multi-input-container") ]
                            [ input
                                [ id "smart-select-input"
                                , class (classPrefix ++ "multi-input")
                                , autocomplete False
                                , onInput <| \val -> model.settings.internalMsg <| SetSearchText val
                                ]
                                []
                            ]
                         ]
                            |> List.append (List.map (selectedEntityWrapper model selectedViewFn) model.selected)
                        )

                  else
                    div [ class (classPrefix ++ "multi-selected-container") ]
                        (List.map (selectedEntityWrapper model selectedViewFn) model.selected)

                -- figure out alignment issue if possible instead of using 'left -1px'
                , if model.isOpen then
                    div
                        [ style "width" (String.fromFloat model.selectWidth ++ "px")
                        , style "left" "-1px"
                        , classList
                            [ ( String.join " " [ classPrefix ++ "options-container", classPrefix ++ "multi-bg-color" ], True )
                            , ( classPrefix ++ "invisible", model.selectWidth == 0 )
                            ]
                        ]
                        [ viewRemoteData model ]

                  else
                    text ""
                ]
            ]
