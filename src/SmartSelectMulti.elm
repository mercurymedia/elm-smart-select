module SmartSelectMulti exposing
    ( SmartSelect, Msg, init, view, subscriptions, update
    , Settings
    , selected
    , setSelected
    )

{-| A select component for multi selection.


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
import SmartSelect.Utilities as Utilities exposing (KeyCode(..), SearchUnion(..))
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
    , localResults : List a
    , remoteResults : RemoteData ( String, String ) (List a)
    , focusedIndex : Int
    }


{-| The type facilitating the configuration of the smart select.

  - The `internalMsg` field takes a function that expects a SmartSelect.Msg and returns an externally defined msg.
  - `optionType` is a string that indicates what kind of data is being selected, i.e. "Product" or "Client"
  - `optionLabel` expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose"
  - `optionDescription` expects an instance of the data being selected from and returns a string describing the instance, i.e. if the label is "Garden Hose", the description may be "30 ft"
      - Because the smart select is unaware of the type and structure of the data it is processing, these functions are necessary to help render the options in the select dropdown.
  - The `searchFn` field expects a `SearchUnion`.
  - `debounceDuration` indicates how long if at all to wait between the last keypress and executing a search. This is particularly useful if the search being executed is pinging an external source.
  - `characterThreshold` indicates how many if any characters should be typed before a search is executed.
  - `closeOnSelect` indicates whether or not the `SmartSelect` should close itself after a selection has been made.

-}
type alias Settings msg a =
    { internalMsg : Msg a -> msg
    , optionType : String
    , optionLabel : a -> String
    , optionDescription : a -> String
    , searchFn : SearchUnion a
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
    | GotLocalSearchResults (List a)
    | GotApiSearchResults (RemoteData Http.Error (List a))
    | WindowResized ( Int, Int )
    | MaybeGotSelect (Result Dom.Error Element)
    | DismissError
    | Open
    | Close


{-| Instantiates and returns a smart select. Takes in the select configuration and a previously selected element, if any.
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
        , localResults = []
        , remoteResults = NotAsked
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
            , case model.settings.searchFn of
                API _ ->
                    case model.remoteResults of
                        NotAsked ->
                            if model.settings.characterSearchThreshold == 0 then
                                Sub.map (\sMsg -> model.settings.internalMsg <| SpinnerMsg sMsg) Spinner.subscription

                            else
                                Sub.none

                        Loading ->
                            Sub.map (\sMsg -> model.settings.internalMsg <| SpinnerMsg sMsg) Spinner.subscription

                        _ ->
                            Sub.none

                Local _ ->
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


localOrRemoteResults : SmartSelect msg a -> List a
localOrRemoteResults (SmartSelect model) =
    case model.settings.searchFn of
        API _ ->
            case model.remoteResults of
                Success results ->
                    results

                _ ->
                    []

        Local _ ->
            model.localResults


selectableEntitiesWithoutSelected : List a -> List a -> List a
selectableEntitiesWithoutSelected selectable selectedEntities =
    List.filter (\el -> not <| List.member el selectedEntities) selectable


keyActionMapper : SmartSelect msg a -> Decode.Decoder ( msg, Bool )
keyActionMapper (SmartSelect model) =
    let
        options =
            localOrRemoteResults (SmartSelect model)

        selectableEntities =
            selectableEntitiesWithoutSelected options model.selected
                |> List.indexedMap Tuple.pair
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
                                if model.focusedIndex + 1 > (List.length selectableEntities - 1) then
                                    List.length selectableEntities - 1

                                else
                                    model.focusedIndex + 1
                        in
                        ( model.settings.internalMsg <| DownKeyPressed newIdx, Utilities.preventDefault key )

                    Enter ->
                        case Dict.get model.focusedIndex (Dict.fromList selectableEntities) of
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
                ( SmartSelect { model | isOpen = False, searchText = "", selected = newSelected, localResults = [], remoteResults = NotAsked }, Cmd.none )

            else
                ( SmartSelect { model | focusedIndex = idx, selected = newSelected }, focusInput model.settings )

        HandleDeselection newSelected ->
            ( SmartSelect { model | selected = newSelected }, focusInput model.settings )

        UpKeyPressed idx ->
            ( SmartSelect { model | focusedIndex = idx }, Cmd.none )

        DownKeyPressed idx ->
            ( SmartSelect { model | focusedIndex = idx }, Cmd.none )

        SetSearchText text ->
            if String.length text < model.settings.characterSearchThreshold then
                ( SmartSelect { model | searchText = text, remoteResults = NotAsked }, Cmd.none )

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
            case model.settings.searchFn of
                API _ ->
                    ( SmartSelect { model | debounce = debounce, remoteResults = Loading }, cmd )

                Local _ ->
                    ( SmartSelect { model | debounce = debounce }, cmd )

        SpinnerMsg spinnerMsg ->
            let
                spinnerModel =
                    Spinner.update spinnerMsg model.spinner
            in
            ( SmartSelect { model | spinner = spinnerModel }
            , Cmd.none
            )

        GotLocalSearchResults results ->
            ( SmartSelect { model | localResults = results, focusedIndex = 0 }, focusInput model.settings )

        GotApiSearchResults result ->
            ( SmartSelect { model | focusedIndex = 0, remoteResults = RemoteData.mapError (Errors.httpErrorToReqErrTuple "GET") result }, Cmd.none )

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
            case model.remoteResults of
                Failure _ ->
                    ( SmartSelect { model | remoteResults = NotAsked }, Cmd.none )

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
            ( SmartSelect { model | isOpen = False, searchText = "", localResults = [], remoteResults = NotAsked }, Cmd.none )


search : Settings msg a -> String -> Cmd msg
search { searchFn, internalMsg } searchText =
    case searchFn of
        Local localSearch ->
            Task.perform (\results -> internalMsg <| GotLocalSearchResults results) (Task.succeed (localSearch searchText))

        API ( _, attrs ) ->
            Http.request
                { method = "GET"
                , headers = attrs.headers
                , url = attrs.url searchText
                , body = Http.emptyBody
                , expect = Http.expectJson (\results -> RemoteData.fromResult results |> (\remoteData -> internalMsg <| GotApiSearchResults remoteData)) (Utilities.decodeOptions attrs.optionDecoder)
                , timeout = Nothing
                , tracker = Nothing
                }


focusInput : Settings msg a -> Cmd msg
focusInput settings =
    Task.attempt (\_ -> settings.internalMsg NoOp) (Dom.focus "smart-select-input")


getSelectWidth : Settings msg a -> Cmd msg
getSelectWidth settings =
    Task.attempt (\select -> settings.internalMsg (MaybeGotSelect select)) (Dom.getElement "smart-select-component")


classPrefix : String
classPrefix =
    "elm-smart-select--"


showSpinner : Model msg a -> Html msg
showSpinner model =
    case model.settings.searchFn of
        API ( spinnerColor, _ ) ->
            div [ class (classPrefix ++ "loading-spinner-container") ] [ div [ class (classPrefix ++ "loading-spinner") ] [ Spinner.view (Utilities.spinnerConfig spinnerColor) model.spinner ] ]

        _ ->
            text ""


showOptions : Model msg a -> List a -> Html msg
showOptions model options =
    let
        selectableEntities =
            selectableEntitiesWithoutSelected options model.selected
                |> List.indexedMap Tuple.pair
    in
    if List.isEmpty selectableEntities then
        div [ class (classPrefix ++ "search-or-no-results-text") ] [ text ("No results found for: " ++ model.searchText) ]

    else
        div [ class (classPrefix ++ "select-options-container") ]
            (List.map
                (\( idx, opt ) ->
                    div
                        [ Events.stopPropagationOn "click" (Decode.succeed ( model.settings.internalMsg <| HandleSelection <| ( Utilities.newFocusedIndexAfterSelection model.focusedIndex, opt :: model.selected ), True ))
                        , onMouseEnter <| model.settings.internalMsg <| SetFocused idx
                        , classList
                            [ ( classPrefix ++ "select-option", True ), ( classPrefix ++ "select-option-focused", idx == model.focusedIndex ) ]
                        ]
                        [ div [] [ text (model.settings.optionLabel opt) ]
                        , div
                            [ classList
                                [ ( classPrefix ++ "select-option-description", True )
                                , ( classPrefix ++ "select-option-description-unfocused", idx /= model.focusedIndex )
                                , ( classPrefix ++ "select-option-description-focused", idx == model.focusedIndex )
                                ]
                            ]
                            [ text (model.settings.optionDescription opt) ]
                        ]
                )
                selectableEntities
            )


viewResults : SmartSelect msg a -> Html msg
viewResults (SmartSelect model) =
    case model.settings.searchFn of
        API _ ->
            case model.remoteResults of
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

                Success results ->
                    showOptions model results

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

        Local _ ->
            showOptions model model.localResults


selectedEntityWrapper : Model msg a -> (a -> Html msg) -> a -> Html msg
selectedEntityWrapper model selectedViewFn entity =
    div
        [ class (classPrefix ++ "selected-entity-wrapper"), Events.stopPropagationOn "click" (Decode.succeed ( model.settings.internalMsg <| HandleDeselection <| List.filter (\e -> e /= entity) model.selected, True )) ]
        [ selectedViewFn entity ]


{-| The smart select view for selecting multiple options at a time. It expects the following arguments (in order):

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
            , Events.preventDefaultOn "keydown" (keyActionMapper (SmartSelect model))
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
                        [ viewResults (SmartSelect model) ]

                  else
                    text ""
                ]
            ]
