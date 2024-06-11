module MultiSelectRemote exposing (SmartSelect, Msg, init, view, viewCustom, subscriptions, update)

{-| A select component for multi selection with remote data.


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
import SmartSelect.ViewComponents exposing (classPrefix, viewEmptyOptionsListItem, viewError, viewOptionsList, viewOptionsListItem, viewTextField, viewTextFieldContainer)
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
    , remoteData : RemoteData ( String, String ) (List a)
    , focusedOptionIndex : Int
    , selectionMsg : ( List a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , characterSearchThreshold : Int
    , debounceDuration : Float
    , idPrefix : Prefix
    , alignment : Maybe Alignment
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
  - `characterSearchThreshold` takes an integer that specifies how many characters need to be typed before triggering the remote query.
  - `debounceDuration` takes a float that specifies the duration in milliseconds between the last keypress and remote query being triggered.

-}
init : { selectionMsg : ( List a, Msg a ) -> msg, internalMsg : Msg a -> msg, characterSearchThreshold : Int, debounceDuration : Float, idPrefix : String } -> SmartSelect msg a
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
            , Browser.Events.onMouseDown (clickedOutsideSelect (Id.select model.idPrefix) model.internalMsg)
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
            ( SmartSelect { model | focusedOptionIndex = idx }, focusInput model.idPrefix model.internalMsg )

        UpKeyPressed idx ->
            ( SmartSelect { model | focusedOptionIndex = idx }, scrollToOption model.internalMsg model.idPrefix idx )

        DownKeyPressed idx ->
            ( SmartSelect { model | focusedOptionIndex = idx }, scrollToOption model.internalMsg model.idPrefix idx )

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
            ( SmartSelect { model | focusedOptionIndex = 0, remoteData = RemoteData.mapError (Errors.httpErrorToReqErrTuple "GET") data }, Cmd.none )

        WindowResized _ ->
            ( SmartSelect model, getAlignment model.idPrefix model.internalMsg )

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
            ( SmartSelect { model | focusedOptionIndex = focusedOptionIndex }
            , Cmd.batch [ getAlignment model.idPrefix model.internalMsg, focusInput model.idPrefix model.internalMsg ]
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
                    if model.characterSearchThreshold == 0 then
                        search { remoteQueryAttrs = remoteQueryAttrs, internalMsg = model.internalMsg } ""

                    else
                        Cmd.none

                ( updatedModel, popoverCmd ) =
                    openPopover (SmartSelect model) ""
            in
            ( updatedModel, Cmd.batch [ popoverCmd, cmd ] )

        Close ->
            ( SmartSelect { model | isOpen = False, searchText = "", alignment = Nothing, remoteData = NotAsked }
            , blurInput model.idPrefix model.internalMsg
            )

        Clear ->
            openPopover (SmartSelect { model | remoteData = NotAsked }) ""


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


openPopover : SmartSelect msg a -> String -> ( SmartSelect msg a, Cmd msg )
openPopover (SmartSelect model) searchText =
    ( SmartSelect { model | isOpen = True, searchText = searchText, focusedOptionIndex = 0 }
    , Cmd.batch
        [ getAlignment model.idPrefix model.internalMsg
        , focusInput model.idPrefix model.internalMsg
        ]
    )


focusInput : Prefix -> (Msg a -> msg) -> Cmd msg
focusInput prefix internalMsg =
    Task.attempt (\_ -> internalMsg NoOp) (Dom.focus (Id.input prefix))


blurInput : Prefix -> (Msg a -> msg) -> Cmd msg
blurInput prefix internalMsg =
    Task.attempt (\_ -> internalMsg NoOp) (Dom.blur (Id.input prefix))


getAlignment : Prefix -> (Msg a -> msg) -> Cmd msg
getAlignment prefix internalMsg =
    Task.attempt (\alignment -> internalMsg (GotAlignment alignment))
        (Alignment.getElements (Id.container prefix) (Id.select prefix))


scrollToOption : (Msg a -> msg) -> Prefix -> Int -> Cmd msg
scrollToOption internalMsg prefix idx =
    Task.attempt (\_ -> internalMsg NoOp) (scrollTask prefix idx)


scrollTask : Prefix -> Int -> Task.Task Dom.Error ()
scrollTask prefix idx =
    Task.sequence
        [ Dom.getElement (Id.option prefix idx) |> Task.map (\x -> x.element.y)
        , Dom.getElement (Id.option prefix idx) |> Task.map (\x -> x.element.height)
        , Dom.getElement (Id.container prefix) |> Task.map (\x -> x.element.y)
        , Dom.getElement (Id.container prefix) |> Task.map (\x -> x.element.height)
        , Dom.getViewportOf (Id.container prefix) |> Task.map (\x -> x.viewport.y)
        ]
        |> Task.andThen
            (\outcome ->
                case outcome of
                    optionY :: optionHeight :: containerY :: containerHeight :: containerScrollTop :: [] ->
                        if (optionY + optionHeight) >= containerY + containerHeight then
                            Dom.setViewportOf (Id.container prefix) 0 (containerScrollTop + ((optionY - (containerY + containerHeight)) + optionHeight))
                                |> Task.onError (\_ -> Task.succeed ())

                        else if optionY < containerY then
                            Dom.setViewportOf (Id.container prefix) 0 (containerScrollTop + (optionY - containerY))
                                |> Task.onError (\_ -> Task.succeed ())

                        else
                            Task.succeed ()

                    _ ->
                        Task.succeed ()
            )


showSpinner : { spinner : Spinner.Model, spinnerColor : Color.Color } -> Html msg
showSpinner { spinner, spinnerColor } =
    div [ class (classPrefix "loading-spinner-container") ] [ div [ class (classPrefix "loading-spinner") ] [ Spinner.view (Utilities.spinnerConfig spinnerColor) spinner ] ]


showOptions :
    { selectionMsg : ( List a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , focusedOptionIndex : Int
    , searchText : String
    , selectedOptions : List a
    , options : List ( Int, a )
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    , idPrefix : Prefix
    }
    -> Html msg
showOptions { selectionMsg, internalMsg, focusedOptionIndex, searchText, selectedOptions, options, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight, noResultsForMsg, noOptionsMsg, idPrefix } =
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
                    viewOptionsListItem
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
    , characterSearchThreshold : Int
    , searchText : String
    , selectedOptions : List a
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
viewRemoteData { selectionMsg, internalMsg, focusedOptionIndex, characterSearchThreshold, searchText, selectedOptions, remoteData, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight, spinner, spinnerColor, characterThresholdPrompt, queryErrorMsg, noResultsForMsg, noOptionsMsg, idPrefix } =
    case remoteData of
        NotAsked ->
            if characterSearchThreshold == 0 then
                showSpinner { spinner = spinner, spinnerColor = spinnerColor }

            else
                let
                    difference =
                        characterSearchThreshold - String.length searchText

                    searchPrompt =
                        if difference == 0 then
                            showSpinner { spinner = spinner, spinnerColor = spinnerColor }

                        else
                            div [ class (classPrefix "search-prompt") ] [ text <| characterThresholdPrompt difference ]
                in
                div [ class (classPrefix "search-prompt-container") ] [ searchPrompt ]

        Loading ->
            showSpinner { spinner = spinner, spinnerColor = spinnerColor }

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
                , optionsContainerMaxHeight = optionsContainerMaxHeight
                , noResultsForMsg = noResultsForMsg
                , noOptionsMsg = noOptionsMsg
                , idPrefix = idPrefix
                }

        Failure _ ->
            viewError []
                { message = queryErrorMsg, onDismiss = internalMsg DismissError }


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
    -> Html msg
selectedEntityWrapper { selectionMsg, viewSelectedOptionFn, selectedOptions } selectedOption =
    div
        [ Events.stopPropagationOn "click" (Decode.succeed ( selectionMsg ( List.filter (\e -> e /= selectedOption) selectedOptions, SelectionChanged Nothing ), True )) ]
        [ viewSelectedOptionFn selectedOption ]


{-| The smart select view for selecting multiple options at a time with local data.

  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".
  - `viewSelectedOptionFn` takes a function that expects an instance of the data being selected from and returns html to render a selected option.

-}
view : { selected : List a, optionLabelFn : a -> String, viewSelectedOptionFn : a -> Html msg } -> SmartSelect msg a -> Html msg
view { selected, optionLabelFn, viewSelectedOptionFn } smartSelect =
    let
        config =
            { isDisabled = False
            , selected = selected
            , optionLabelFn = optionLabelFn
            , optionDescriptionFn = \_ -> ""
            , viewSelectedOptionFn = viewSelectedOptionFn
            , optionsContainerMaxHeight = 300
            , spinnerColor = Color.rgb255 57 179 181
            , selectTitle = "Placeholder..."
            , characterThresholdPrompt = \_ -> "Enter at least 2 characters to search..."
            , queryErrorMsg = "Error"
            , noResultsForMsg = \_ -> "No results found"
            , noOptionsMsg = "No options available"
            }
    in
    viewCustom config smartSelect


{-| The smart select view for selecting multiple options at a time with local data.

  - `isDisabled` takes a boolean that indicates whether or not the select can be opened.
  - `selected` takes a list of the currently selected entities.
  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".
  - `optionDescriptionFn` takes a function that expects an instance of the data being selected from and returns a string describing the instance, i.e. if the label is "Garden Hose", the description may be "30 ft".
  - `viewSelectedOptionFn` takes a function that expects and instance of the data being selected from and returns html to render a selected option.
  - `optionsContainerMaxHeight` takes a float that specifies the max height of the container of the selectable options.
  - `spinnerColor` takes a `Color` for the loading spinner.
  - `selectTitle` takes a string to label the select in its closed state and non-selected state.
  - `characterThresholdPrompt` takes a function that expects an int and returns a string indicating how many more characters need to be entered to trigger the query.
  - `queryErrorMsg` takes a string to indicate that an error has occured while querying data.
  - `noResultsForMsg` takes a function that expects a string and returns a message indicating that the search for the provided string returned no results.
  - `noOptionsMsg` takes a string to indicate that no options exist in the select.

```elm
import MultiSelectRemote
import Html exposing (Html)
import Color

type Msg
    = HandleSelectUpdate (MultiSelectRemote.Msg Product)
    | HandleSelection ( List Product, MultiSelectRemote.Msg Product )

type alias Product =
    { name : String
    , description : String
    , price : Float
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { products = exampleProducts
      , select =
            MultiSelectRemote.init
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
    , select : MultiSelectRemote.SmartSelect Msg Product
    , selectedProducts : List Product
    }

viewSelectedProduct : Product -> Html Msg
viewSelectedProduct product =
    div []
        [ text (product.name ++ " - " ++ ("$" ++ String.fromFloat product.price)) ]

viewCustomProductSelect : Model -> Html Msg
viewCustomProductSelect model =
    MultiSelectRemote.viewCustom
        { isDisabled = False
        , selected = model.selectedProducts
        , optionLabelFn = .name
        , optionDescriptionFn = \option -> "$" ++ String.fromFloat option.price
        , viewSelectedOptionFn = viewSelecteProduct
        , optionsContainerMaxHeight = 500
        , spinnerColor = Color.rgb255 0 0 0
        , selectTitle = "Select a Product"
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
viewCustom :
    { isDisabled : Bool
    , selected : List a
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , viewSelectedOptionFn : a -> Html msg
    , optionsContainerMaxHeight : Float
    , spinnerColor : Color.Color
    , selectTitle : String
    , characterThresholdPrompt : Int -> String
    , queryErrorMsg : String
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    }
    -> SmartSelect msg a
    -> Html msg
viewCustom { isDisabled, selected, optionLabelFn, optionDescriptionFn, viewSelectedOptionFn, optionsContainerMaxHeight, spinnerColor, selectTitle, characterThresholdPrompt, queryErrorMsg, noResultsForMsg, noOptionsMsg } (SmartSelect model) =
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
                , selectedOptions = selected
                , focusedOptionIndex = model.focusedOptionIndex
                , selectionMsg = model.selectionMsg
                , internalMsg = model.internalMsg
                , searchText = model.searchText
                }
            )
        ]
        [ viewTextField [ onClick <| model.internalMsg Open ]
            { inputAttributes =
                [ id (Id.input model.idPrefix)
                , autocomplete False
                , onInput <| \val -> model.internalMsg <| SetSearchText val
                , value model.searchText
                , placeholder selectTitle
                ]
            , isDisabled = isDisabled
            , selectedOptions =
                List.map
                    (selectedEntityWrapper
                        { selectionMsg = model.selectionMsg
                        , viewSelectedOptionFn = viewSelectedOptionFn
                        , selectedOptions = selected
                        }
                    )
                    selected
            , clearIconAttributes = [ Events.stopPropagationOn "click" (Decode.succeed ( model.selectionMsg ( [], SelectionChanged Nothing ), True )) ]
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
                , selectedOptions = selected
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
