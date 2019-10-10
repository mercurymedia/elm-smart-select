module SingleSelectLocal exposing
    ( SmartSelect, Msg, init, view, subscriptions, update
    , Settings
    , selected
    , setSelected
    )

{-| A select component for a single selection with local data.


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
import Dict
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (autocomplete, class, classList, id, placeholder, style)
import Html.Events as Events exposing (onClick, onInput, onMouseEnter)
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..))
import SmartSelect.Utilities as Utilities exposing (KeyCode(..))
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
    , selected : Maybe a
    , focusedIndex : Int
    }


{-| The type facilitating the configuration of the smart select.

  - The `internalMsg` field takes a function that expects a SmartSelect.Msg and returns an externally defined msg.
  - `optionType` is a string that indicates what kind of data is being selected, i.e. "Product" or "Client"
  - `optionLabel` expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose"
  - `optionDescription` expects an instance of the data being selected from and returns a string describing the instance, i.e. if the label is "Garden Hose", the description may be "30 ft"
      - Because the smart select is unaware of the type and structure of the data it is processing, these functions are necessary to help render the options in the select dropdown.
  - The `optionsContainerMaxHeight` and `optionHeight` fields specify the height of the container of the selectable options and the options themselves, respectively. These fields help facilitate scroll functionality.
  - The `searchFn` field expects a function taking the search text and the items to search and returning the filtered items.
  - `closeOnSelect` indicates whether or not the `SmartSelect` should close itself after a selection has been made.

-}
type alias Settings msg a =
    { internalMsg : Msg a -> msg
    , optionType : String
    , optionLabel : a -> String
    , optionDescription : a -> String
    , optionsContainerMaxHeight : Float
    , optionHeight : Float
    , searchFn : String -> List a -> List a
    , closeOnSelect : Bool
    }


{-| Opaque type representing cases to be passed to SmartSelect.update
-}
type Msg a
    = NoOp
    | SetFocused Int
    | HandleSelection ( Int, a )
    | UpKeyPressed Int
    | DownKeyPressed Int
    | SetSearchText String
    | WindowResized ( Int, Int )
    | MaybeGotSelect (Result Dom.Error Element)
    | Open
    | Close


{-| Instantiates and returns a smart select. Takes in the select configuration and a list of previously selected elements, if any.
-}
init : Settings msg a -> Maybe a -> SmartSelect msg a
init settings alreadySelected =
    SmartSelect
        { settings = settings
        , selectWidth = 0
        , isOpen = False
        , searchText = ""
        , selected = alreadySelected
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


keyActionMapper : Model msg a -> List ( Int, a ) -> Decode.Decoder ( msg, Bool )
keyActionMapper model options =
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
                                ( model.settings.internalMsg <| HandleSelection <| ( Utilities.newFocusedIndexAfterSelection model.focusedIndex, item ), Utilities.preventDefault key )

                            Nothing ->
                                ( model.settings.internalMsg NoOp, Utilities.preventDefault key )

                    Escape ->
                        ( model.settings.internalMsg Close, Utilities.preventDefault key )

                    Other ->
                        ( model.settings.internalMsg NoOp, Utilities.preventDefault key )
            )


{-| Get the currently selected entity if any.
-}
selected : SmartSelect msg a -> Maybe a
selected (SmartSelect model) =
    model.selected


{-| It is possible that the select is instantiated on your model before data representing
a previous selection is loaded. Use this function to update the picked selection in
the select when the appropriate data is received. Use this method sparingly, if at all.
The selected state should ideally only change due to user input.
-}
setSelected : a -> SmartSelect msg a -> SmartSelect msg a
setSelected newSelected (SmartSelect model) =
    SmartSelect { model | selected = Just newSelected }


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
                ( SmartSelect { model | isOpen = False, searchText = "", selected = Just newSelected }, Cmd.none )

            else
                ( SmartSelect { model | focusedIndex = idx, selected = Just newSelected }, focusInput model.settings )

        UpKeyPressed idx ->
            ( SmartSelect { model | focusedIndex = idx }, scrollToOption model.settings idx )

        DownKeyPressed idx ->
            ( SmartSelect { model | focusedIndex = idx }, scrollToOption model.settings idx )

        SetSearchText text ->
            ( SmartSelect { model | searchText = text, focusedIndex = 0 }, Cmd.none )

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

        Open ->
            ( SmartSelect { model | isOpen = True, focusedIndex = 0 }, Cmd.batch [ getSelectWidth model.settings, focusInput model.settings ] )

        Close ->
            ( SmartSelect { model | isOpen = False, searchText = "" }, Cmd.none )


focusInput : Settings msg a -> Cmd msg
focusInput settings =
    Task.attempt (\_ -> settings.internalMsg NoOp) (Dom.focus "smart-select-input")


getSelectWidth : Settings msg a -> Cmd msg
getSelectWidth settings =
    Task.attempt (\select -> settings.internalMsg <| MaybeGotSelect select) (Dom.getElement smartSelectId)


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


showOptions : Model msg a -> List ( Int, a ) -> Html msg
showOptions model options =
    if List.isEmpty options && model.searchText /= "" then
        div [ class (classPrefix ++ "search-or-no-results-text") ] [ text ("No results found for: " ++ model.searchText) ]

    else if List.isEmpty options then
        div [ class (classPrefix ++ "search-or-no-results-text") ] [ text ("No " ++ model.settings.optionType ++ "s are available") ]

    else
        div [ id (classPrefix ++ "select-options-container"), style "max-height" (String.fromFloat model.settings.optionsContainerMaxHeight ++ "px"), style "overflow" "auto" ]
            (List.map
                (\( idx, option ) ->
                    div
                        [ Events.stopPropagationOn "click" (Decode.succeed ( model.settings.internalMsg <| HandleSelection ( Utilities.newFocusedIndexAfterSelection model.focusedIndex, option ), True ))
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


removeSelectedFromOptions : Maybe a -> List a -> List a
removeSelectedFromOptions selectedOption options =
    Maybe.map (\s -> List.filter (\el -> el /= s) options) selectedOption
        |> Maybe.withDefault options


filterAndIndexOptions : Model msg a -> List a -> List ( Int, a )
filterAndIndexOptions model unfilteredOptions =
    if model.searchText == "" then
        removeSelectedFromOptions model.selected unfilteredOptions
            |> List.indexedMap Tuple.pair

    else
        model.settings.searchFn model.searchText unfilteredOptions
            |> removeSelectedFromOptions model.selected
            |> List.indexedMap Tuple.pair


{-| The smart select view for selecting one option at a time with local data. It expects the following arguments (in order):

  - a boolean indicating if the select is disabled or not
  - a list of the items from which to select
  - the smart select instance

-}
view : Bool -> List a -> SmartSelect msg a -> Html msg
view isDisabled allOptions (SmartSelect model) =
    let
        selectedLabel =
            Maybe.map (\s -> model.settings.optionLabel s) model.selected |> Maybe.withDefault model.settings.optionType
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
            , onClick <| model.settings.internalMsg Open
            , Events.preventDefaultOn "keydown" (keyActionMapper model <| filterAndIndexOptions model allOptions)
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
                            [ input
                                [ id "smart-select-input"
                                , class (classPrefix ++ "single-selector-input")
                                , autocomplete False
                                , onInput <| \val -> model.settings.internalMsg <| SetSearchText val
                                , placeholder <| "Search for a " ++ model.settings.optionType
                                ]
                                []
                            ]
                        , showOptions model <| filterAndIndexOptions model allOptions
                        ]

                  else
                    text ""
                ]
            ]
