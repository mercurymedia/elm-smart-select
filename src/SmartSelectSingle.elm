module SmartSelectSingle exposing
    ( SmartSelect, Msg, init, view, subscriptions, update
    , Settings
    )

{-| A select component for a single selection.


# Architecture

@docs SmartSelect, Msg, init, view, subscriptions, update


# Settings and Configuration

@docs Settings

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
type SmartSelect a
    = SmartSelect (Model a)


type alias Model a =
    { selectWidth : Float
    , isOpen : Bool
    , searchText : String
    , debounce : Debounce String
    , spinner : Spinner.Model
    , localResults : List a
    , remoteResults : RemoteData ( String, String ) (List a)
    , focusedIndex : Int
    }


{-| The type facilitating the configuration of the smart select.

  - The `internalMsg` field takes a function that expects a tuple containing a SmartSelect.Msg as well as the selection state and returns an externally defined msg.
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
    { internalMsg : ( Msg a, Maybe a ) -> msg
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
    | HandleSelection Int
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


{-| Instantiates and returns a smart select.
-}
init : SmartSelect a
init =
    SmartSelect
        { selectWidth = 0
        , isOpen = False
        , searchText = ""
        , debounce = Debounce.init
        , spinner = Spinner.init
        , localResults = []
        , remoteResults = NotAsked
        , focusedIndex = 0
        }


smartSelectId : String
smartSelectId =
    "smart-select-component"


{-| Events external to the smart select to which it is subscribed.
-}
subscriptions : Settings msg a -> Maybe a -> SmartSelect a -> Sub msg
subscriptions settings selected (SmartSelect model) =
    if model.isOpen then
        Sub.batch
            [ Browser.Events.onResize (\h w -> settings.internalMsg ( WindowResized ( h, w ), selected ))
            , case settings.searchFn of
                API _ ->
                    case model.remoteResults of
                        NotAsked ->
                            if settings.characterSearchThreshold == 0 then
                                Sub.map (\sMsg -> settings.internalMsg ( SpinnerMsg sMsg, selected )) Spinner.subscription

                            else
                                Sub.none

                        Loading ->
                            Sub.map (\sMsg -> settings.internalMsg ( SpinnerMsg sMsg, selected )) Spinner.subscription

                        _ ->
                            Sub.none

                Local _ ->
                    Sub.none
            , Browser.Events.onMouseDown (clickedOutsideSelect smartSelectId settings selected)
            ]

    else
        Sub.none


clickedOutsideSelect : String -> Settings msg a -> Maybe a -> Decode.Decoder msg
clickedOutsideSelect componentId settings selected =
    Decode.field "target" (Utilities.eventIsOutsideComponent componentId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| settings.internalMsg ( Close, selected )

                else
                    Decode.fail "inside component"
            )


localOrRemoteResults : Settings msg a -> SmartSelect a -> List a
localOrRemoteResults settings (SmartSelect model) =
    case settings.searchFn of
        API _ ->
            case model.remoteResults of
                Success results ->
                    results

                _ ->
                    []

        Local _ ->
            model.localResults


selectableEntitiesWithoutSelected : List a -> Maybe a -> List a
selectableEntitiesWithoutSelected selectable selected =
    Maybe.map (\s -> List.filter (\el -> el /= s) selectable) selected
        |> Maybe.withDefault selectable


keyActionMapper : Maybe a -> Settings msg a -> SmartSelect a -> Decode.Decoder ( msg, Bool )
keyActionMapper selected settings (SmartSelect model) =
    let
        options =
            localOrRemoteResults settings (SmartSelect model)

        selectableEntities =
            selectableEntitiesWithoutSelected options selected
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
                        ( settings.internalMsg ( UpKeyPressed newIdx, selected ), Utilities.preventDefault key )

                    Down ->
                        let
                            newIdx =
                                if model.focusedIndex + 1 > (List.length selectableEntities - 1) then
                                    List.length selectableEntities - 1

                                else
                                    model.focusedIndex + 1
                        in
                        ( settings.internalMsg ( DownKeyPressed newIdx, selected ), Utilities.preventDefault key )

                    Enter ->
                        case Dict.get model.focusedIndex (Dict.fromList selectableEntities) of
                            Just item ->
                                ( settings.internalMsg ( HandleSelection <| Utilities.newFocusedIndexAfterSelection model.focusedIndex, Just item ), Utilities.preventDefault key )

                            Nothing ->
                                ( settings.internalMsg ( NoOp, selected ), Utilities.preventDefault key )

                    Escape ->
                        ( settings.internalMsg ( Close, selected ), Utilities.preventDefault key )

                    Other ->
                        ( settings.internalMsg ( NoOp, selected ), Utilities.preventDefault key )
            )


debounceConfig : Settings msg a -> Maybe a -> Debounce.Config msg
debounceConfig settings selected =
    { strategy = Debounce.later settings.debounceDuration
    , transform = \debounceMsg -> settings.internalMsg ( DebounceMsg debounceMsg, selected )
    }


{-| Update the provided smart select and receive the updated select instance and a cmd to run.
-}
update : Msg a -> Settings msg a -> Maybe a -> SmartSelect a -> ( SmartSelect a, Cmd msg )
update msg settings selected (SmartSelect model) =
    case msg of
        NoOp ->
            ( SmartSelect model, Cmd.none )

        SetFocused idx ->
            ( SmartSelect { model | focusedIndex = idx }, Cmd.none )

        HandleSelection idx ->
            if settings.closeOnSelect then
                ( SmartSelect { model | isOpen = False, searchText = "", localResults = [], remoteResults = NotAsked }, Cmd.none )

            else
                ( SmartSelect { model | focusedIndex = idx }, focusInput settings selected )

        UpKeyPressed idx ->
            ( SmartSelect { model | focusedIndex = idx }, Cmd.none )

        DownKeyPressed idx ->
            ( SmartSelect { model | focusedIndex = idx }, Cmd.none )

        SetSearchText text ->
            if String.length text < settings.characterSearchThreshold then
                ( SmartSelect { model | searchText = text, remoteResults = NotAsked }, Cmd.none )

            else
                let
                    ( debounce, cmd ) =
                        Debounce.push (debounceConfig settings selected) text model.debounce
                in
                ( SmartSelect { model | searchText = text, debounce = debounce }
                , cmd
                )

        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        (debounceConfig settings selected)
                        (Debounce.takeLast (search settings selected))
                        msg_
                        model.debounce
            in
            case settings.searchFn of
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
            ( SmartSelect { model | localResults = results, focusedIndex = 0 }, focusInput settings selected )

        GotApiSearchResults result ->
            ( SmartSelect { model | focusedIndex = 0, remoteResults = RemoteData.mapError (Errors.httpErrorToReqErrTuple "GET") result }, Cmd.none )

        WindowResized _ ->
            ( SmartSelect model, getSelectWidth settings selected )

        MaybeGotSelect result ->
            case result of
                Ok component ->
                    let
                        selectWidth =
                            component.element |> (\el -> el.width)
                    in
                    ( SmartSelect { model | selectWidth = selectWidth }, focusInput settings selected )

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
                    if settings.characterSearchThreshold == 0 then
                        Cmd.batch [ search settings selected "", getSelectWidth settings selected ]

                    else
                        Cmd.batch [ getSelectWidth settings selected, focusInput settings selected ]
            in
            ( SmartSelect { model | isOpen = True, focusedIndex = 0 }, cmd )

        Close ->
            ( SmartSelect { model | isOpen = False, searchText = "", localResults = [], remoteResults = NotAsked }, Cmd.none )


search : Settings msg a -> Maybe a -> String -> Cmd msg
search { searchFn, internalMsg } selected searchText =
    case searchFn of
        Local localSearch ->
            Task.perform (\results -> internalMsg ( GotLocalSearchResults results, selected )) (Task.succeed (localSearch searchText))

        API ( _, attrs ) ->
            Http.request
                { method = "GET"
                , headers = attrs.headers
                , url = attrs.url searchText
                , body = Http.emptyBody
                , expect = Http.expectJson (\results -> RemoteData.fromResult results |> (\remoteData -> internalMsg ( GotApiSearchResults remoteData, selected ))) (Utilities.decodeOptions attrs.optionDecoder)
                , timeout = Nothing
                , tracker = Nothing
                }


focusInput : Settings msg a -> Maybe a -> Cmd msg
focusInput settings selected =
    Task.attempt (\_ -> settings.internalMsg ( NoOp, selected )) (Dom.focus "smart-select-input")


getSelectWidth : Settings msg a -> Maybe a -> Cmd msg
getSelectWidth settings selected =
    Task.attempt (\select -> settings.internalMsg ( MaybeGotSelect select, selected )) (Dom.getElement "smart-select-component")


classPrefix : String
classPrefix =
    "elm-smart-select--"


showSpinner : Settings msg a -> Model a -> Html msg
showSpinner settings model =
    case settings.searchFn of
        API ( spinnerColor, _ ) ->
            div [ class (classPrefix ++ "loading-spinner-container") ] [ div [ class (classPrefix ++ "loading-spinner") ] [ Spinner.view (Utilities.spinnerConfig spinnerColor) model.spinner ] ]

        _ ->
            text ""


showOptions : Maybe a -> Model a -> Settings msg a -> List a -> Html msg
showOptions selected model settings options =
    let
        selectableEntities =
            selectableEntitiesWithoutSelected options selected |> List.indexedMap Tuple.pair
    in
    if List.isEmpty selectableEntities then
        div [ class (classPrefix ++ "search-or-no-results-text") ] [ text ("No results found for: " ++ model.searchText) ]

    else
        div [ class (classPrefix ++ "select-options-container") ]
            (List.map
                (\( idx, option ) ->
                    div
                        [ Events.stopPropagationOn "click" (Decode.succeed ( settings.internalMsg ( HandleSelection <| Utilities.newFocusedIndexAfterSelection model.focusedIndex, Just option ), True ))
                        , onMouseEnter <| settings.internalMsg ( SetFocused idx, selected )
                        , classList
                            [ ( classPrefix ++ "select-option", True ), ( classPrefix ++ "select-option-focused", idx == model.focusedIndex ) ]
                        ]
                        [ div [] [ text (settings.optionLabel option) ]
                        , div
                            [ classList
                                [ ( classPrefix ++ "select-option-description", True )
                                , ( classPrefix ++ "select-option-description-unfocused", idx /= model.focusedIndex )
                                , ( classPrefix ++ "select-option-description-focused", idx == model.focusedIndex )
                                ]
                            ]
                            [ text (settings.optionDescription option) ]
                        ]
                )
                selectableEntities
            )


viewResults : Maybe a -> SmartSelect a -> Settings msg a -> Html msg
viewResults selected (SmartSelect model) settings =
    case settings.searchFn of
        API _ ->
            case model.remoteResults of
                NotAsked ->
                    if settings.characterSearchThreshold == 0 then
                        showSpinner settings model

                    else
                        let
                            difference =
                                settings.characterSearchThreshold - String.length model.searchText

                            searchPrompt =
                                if settings.characterSearchThreshold > 0 && difference == 0 then
                                    showSpinner settings model

                                else if difference > 1 then
                                    div [ class (classPrefix ++ "search-prompt") ] [ text <| "Please enter " ++ String.fromInt difference ++ " more characters to search for a " ++ String.toLower settings.optionType ]

                                else
                                    div [ class (classPrefix ++ "search-prompt") ] [ text <| "Please enter 1 more character to search for a " ++ String.toLower settings.optionType ]
                        in
                        div [ class (classPrefix ++ "search-prompt-container") ] [ searchPrompt ]

                Loading ->
                    showSpinner settings model

                Success results ->
                    showOptions selected model settings results

                Failure ( requestDecorator, errMsg ) ->
                    div [ class (classPrefix ++ "error-box-container") ]
                        [ div [ class (classPrefix ++ "error-box") ]
                            [ div [ class (classPrefix ++ "error-container") ]
                                [ div [ class (classPrefix ++ "request-decorator") ] [ text requestDecorator ]
                                , div [] [ text errMsg ]
                                ]
                            , span
                                [ class (classPrefix ++ "dismiss-error-x")
                                , onClick <| settings.internalMsg ( DismissError, selected )
                                ]
                                [ Icons.x
                                    |> Icons.withSize 12
                                    |> Icons.withStrokeWidth 4
                                    |> Icons.toHtml []
                                ]
                            ]
                        ]

        Local _ ->
            showOptions selected model settings model.localResults


{-| The smart select view for selecting one option at a time. It expects the following arguments (in order):

  - a boolean indicating if the select is disabled or not
  - the currently selected entity itself
  - the configured settings
  - the smart select instance

-}
view : Bool -> Maybe a -> Settings msg a -> SmartSelect a -> Html msg
view isDisabled selectedEntity settings (SmartSelect model) =
    let
        selectedLabel =
            Maybe.map (\selected -> settings.optionLabel selected) selectedEntity |> Maybe.withDefault settings.optionType
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
            , onClick <| settings.internalMsg ( Open, selectedEntity )
            , Events.preventDefaultOn "keydown" (keyActionMapper selectedEntity settings (SmartSelect model))
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
                            [ input [ id "smart-select-input", class (classPrefix ++ "single-selector-input"), autocomplete False, onInput <| \val -> settings.internalMsg ( SetSearchText val, selectedEntity ) ] [] ]
                        , viewResults selectedEntity (SmartSelect model) settings
                        ]

                  else
                    text ""
                ]
            ]
