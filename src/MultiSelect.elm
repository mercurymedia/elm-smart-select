module MultiSelect exposing
    ( SmartSelect, Msg, init, view, viewCustom, subscriptions, update
    , selected
    , setSelected
    )

{-| A select component for multi selection with local data.


# Architecture

@docs SmartSelect, Msg, init, view, viewCustom, subscriptions, update


# Query

@docs selected


# Additional configuration

@docs setSelected

-}

import Browser.Dom as Dom exposing (Element)
import Browser.Events
import Dict
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (autocomplete, class, classList, id, style, value)
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
    { selectWidth : Float
    , isOpen : Bool
    , searchText : String
    , selected : List a
    , focusedOptionIndex : Int
    , internalMsg : Msg a -> msg
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
    | WindowResized ( Int, Int )
    | MaybeGotSelect (Result Dom.Error Element)
    | Open
    | Close


{-| Instantiates and returns a smart select. Takes a function that expects a SmartSelect.Msg and returns an externally defined msg.
-}
init : (Msg a -> msg) -> SmartSelect msg a
init internalMsg =
    SmartSelect
        { selectWidth = 0
        , isOpen = False
        , searchText = ""
        , selected = []
        , focusedOptionIndex = 0
        , internalMsg = internalMsg
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


keyActionMapper : { options : List ( Int, a ), selectedOptions : List a, focusedOptionIndex : Int, internalMsg : Msg a -> msg } -> Decode.Decoder ( msg, Bool )
keyActionMapper { options, selectedOptions, focusedOptionIndex, internalMsg } =
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
                                ( internalMsg <| HandleSelection <| ( Utilities.newFocusedOptionIndexAfterSelection focusedOptionIndex, item :: selectedOptions ), Utilities.preventDefault key )

                            Nothing ->
                                ( internalMsg NoOp, Utilities.preventDefault key )

                    Escape ->
                        ( internalMsg Close, Utilities.preventDefault key )

                    Other ->
                        ( internalMsg NoOp, Utilities.preventDefault key )
            )


{-| Get the currently selected entities, if any.
-}
selected : SmartSelect msg a -> List a
selected (SmartSelect model) =
    model.selected


{-| It is possible that the select is instantiated on your model before data representing
a previous selection is loaded. Use this function to update the picked selection in
the select when the appropriate data is received.
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
            ( SmartSelect { model | focusedOptionIndex = idx }, Cmd.none )

        HandleSelection ( idx, newSelected ) ->
            ( SmartSelect { model | focusedOptionIndex = idx, selected = newSelected }, focusInput model.internalMsg )

        HandleDeselection newSelected ->
            ( SmartSelect { model | selected = newSelected }, focusInput model.internalMsg )

        UpKeyPressed idx ->
            ( SmartSelect { model | focusedOptionIndex = idx }, scrollToOption model.internalMsg idx )

        DownKeyPressed idx ->
            ( SmartSelect { model | focusedOptionIndex = idx }, scrollToOption model.internalMsg idx )

        SetSearchText text ->
            ( SmartSelect { model | searchText = text, focusedOptionIndex = 0 }, Cmd.none )

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

        Open ->
            ( SmartSelect { model | isOpen = True, focusedOptionIndex = 0 }, Cmd.batch [ getSelectWidth model.internalMsg, focusInput model.internalMsg ] )

        Close ->
            ( SmartSelect { model | isOpen = False, searchText = "" }, Cmd.none )


focusInput : (Msg a -> msg) -> Cmd msg
focusInput internalMsg =
    Task.attempt (\_ -> internalMsg NoOp) (Dom.focus "smart-select-input")


getSelectWidth : (Msg a -> msg) -> Cmd msg
getSelectWidth internalMsg =
    Task.attempt (\select -> internalMsg <| MaybeGotSelect select) (Dom.getElement smartSelectId)


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


showOptions :
    { internalMsg : Msg a -> msg
    , selectedOptions : List a
    , options : List ( Int, a )
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    , searchText : String
    , focusedOptionIndex : Int
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    }
    -> Html msg
showOptions { internalMsg, selectedOptions, options, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight, searchText, focusedOptionIndex, noResultsForMsg, noOptionsMsg } =
    if List.isEmpty options && searchText /= "" then
        div [ class (classPrefix ++ "search-or-no-results-text") ] [ text <| noResultsForMsg searchText ]

    else if List.isEmpty options then
        div [ class (classPrefix ++ "search-or-no-results-text") ] [ text noOptionsMsg ]

    else
        div [ id (classPrefix ++ "select-options-container"), style "max-height" (String.fromFloat optionsContainerMaxHeight ++ "px"), style "overflow" "auto" ]
            (List.map
                (\( idx, option ) ->
                    div
                        [ Events.stopPropagationOn "click" (Decode.succeed ( internalMsg <| HandleSelection ( Utilities.newFocusedOptionIndexAfterSelection focusedOptionIndex, option :: selectedOptions ), True ))
                        , onMouseEnter <| internalMsg <| SetFocused idx
                        , id <| optionId idx
                        , classList
                            [ ( classPrefix ++ "select-option", True ), ( classPrefix ++ "select-option-focused", idx == focusedOptionIndex ) ]
                        ]
                        [ div [] [ text (optionLabelFn option) ]
                        , div
                            [ classList
                                [ ( classPrefix ++ "select-option-description", True )
                                , ( classPrefix ++ "select-option-description-unfocused", idx /= focusedOptionIndex )
                                , ( classPrefix ++ "select-option-description-focused", idx == focusedOptionIndex )
                                ]
                            ]
                            [ text (optionDescriptionFn option) ]
                        ]
                )
                options
            )


removeSelectedFromOptions : List a -> List a -> List a
removeSelectedFromOptions selectedOptions options =
    List.filter (\el -> not <| List.member el selectedOptions) options


filterAndIndexOptions : { options : List a, selectedOptions : List a, searchFn : String -> List a -> List a, searchText : String } -> List ( Int, a )
filterAndIndexOptions { options, selectedOptions, searchFn, searchText } =
    if searchText == "" then
        removeSelectedFromOptions selectedOptions options
            |> List.indexedMap Tuple.pair

    else
        searchFn searchText options
            |> removeSelectedFromOptions selectedOptions
            |> List.indexedMap Tuple.pair


selectedEntityWrapper :
    { internalMsg : Msg a -> msg
    , viewSelectedOptionFn : a -> Html msg
    , selectedOptions : List a
    }
    -> a
    -> Html msg
selectedEntityWrapper { internalMsg, viewSelectedOptionFn, selectedOptions } selectedOption =
    div
        [ class (classPrefix ++ "selected-entity-wrapper"), Events.stopPropagationOn "click" (Decode.succeed ( internalMsg <| HandleDeselection <| List.filter (\e -> e /= selectedOption) selectedOptions, True )) ]
        [ viewSelectedOptionFn selectedOption ]


{-| The smart select view for selecting multiple options at a time with local data.

  - `options` takes a list of the data being selected from.
  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".
  - `viewSelectedOptionFn` takes a function that expects and instance of the data being selected from and returns html to render a selected option.

-}
view : { options : List a, optionLabelFn : a -> String, viewSelectedOptionFn : a -> Html msg } -> SmartSelect msg a -> Html msg
view { options, optionLabelFn, viewSelectedOptionFn } smartSelect =
    let
        config =
            { isDisabled = False
            , options = options
            , optionLabelFn = optionLabelFn
            , optionDescriptionFn = \_ -> ""
            , viewSelectedOptionFn = viewSelectedOptionFn
            , optionsContainerMaxHeight = 300
            , searchFn =
                \searchText allOptions ->
                    List.filter (\option -> String.contains (String.toLower searchText) (String.toLower <| optionLabelFn option)) allOptions
            , selectTitle = ""
            , noResultsForMsg = \_ -> ""
            , noOptionsMsg = ""
            }
    in
    viewCustom config smartSelect


{-| The smart select view for selecting multiple options at a time with local data.

  - `isDisabled` takes a boolean that indicates whether or not the select can be opened.
  - `options` takes a list of the data being selected from.
  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".
  - `optionDescriptionFn` takes a function that expects an instance of the data being selected from and returns a string describing the instance, i.e. if the label is "Garden Hose", the description may be "30 ft".
  - `viewSelectedOptionFn` takes a function that expects an instance of the data being selected from and returns html to render a selected option.
  - `optionsContainerMaxHeight` takes a float that specifies the max height of the container of the selectable options.
  - `searchFn` takes a function that expects the search text and the items to search and returns the filtered items.
  - `selectTitle` takes a string to label the select in its closed state and non-selected state.
  - `noResultsForMsg` takes a function that expects a string and returns a message indicating that the search for the provided string returned no results.
  - `noOptionsMsg` takes a string to indicate that no options exist in the select.

```elm
import MultiSelect
import Html exposing (Html, div)

type Msg
    = ...

type alias Product =
    { name : String
    , description : String
    , price : Float
    }

type alias Model =
    { ...
    , select : MultiSelect.SmartSelect Msg Product
    , products : List Product
    }

viewSelectedProduct : Product -> Html Msg
viewSelectedProduct product =
    div []
        [ text (product.name ++ " - " ++ ("$" ++ String.fromFloat product.price)) ]

viewCustomProductSelect : Model -> Html Msg
viewCustomProductSelect model =
    MultiSelect.viewCustom
        { isDisabled = False
        , options = model.products
        , optionLabelFn = .name
        , optionDescriptionFn = \option -> "$" ++ String.fromFloat option.price
        , viewSelectedOptionFn = viewSelectedProduct
        , optionsContainerMaxHeight = 500
        , searchFn = \searchText allOptions ->
            List.filter (\option ->
                String.contains (String.toLower searchText) (String.toLower option.name) ||
                String.contains (String.toLower searchText) (String.toLower option.description)
            ) allOptions
        , selectTitle = "Select a Product"
        , noResultsForMsg = \searchText -> "No results found for: " ++ searchText
        , noOptionsMsg = "There are no options to select"
        }
```

-}
viewCustom :
    { isDisabled : Bool
    , options : List a
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , viewSelectedOptionFn : a -> Html msg
    , optionsContainerMaxHeight : Float
    , searchFn : String -> List a -> List a
    , selectTitle : String
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    }
    -> SmartSelect msg a
    -> Html msg
viewCustom { isDisabled, options, optionLabelFn, optionDescriptionFn, viewSelectedOptionFn, optionsContainerMaxHeight, searchFn, selectTitle, noResultsForMsg, noOptionsMsg } (SmartSelect model) =
    let
        selectedLabel =
            if List.length model.selected == 0 then
                text selectTitle

            else
                div [ class (classPrefix ++ "multi-selected-container") ]
                    (List.map
                        (selectedEntityWrapper
                            { internalMsg = model.internalMsg
                            , viewSelectedOptionFn = viewSelectedOptionFn
                            , selectedOptions = model.selected
                            }
                        )
                        model.selected
                    )
    in
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
            [ selectedLabel ]

    else
        div
            [ id smartSelectId
            , onClick <| model.internalMsg Open
            , Events.preventDefaultOn "keydown"
                (keyActionMapper
                    { options = filterAndIndexOptions { options = options, selectedOptions = model.selected, searchFn = searchFn, searchText = model.searchText }
                    , selectedOptions = model.selected
                    , focusedOptionIndex = model.focusedOptionIndex
                    , internalMsg = model.internalMsg
                    }
                )
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
                                , onInput <| \val -> model.internalMsg <| SetSearchText val
                                , value model.searchText
                                ]
                                []
                            ]
                         ]
                            |> List.append
                                (List.map
                                    (selectedEntityWrapper
                                        { internalMsg = model.internalMsg
                                        , viewSelectedOptionFn = viewSelectedOptionFn
                                        , selectedOptions = model.selected
                                        }
                                    )
                                    model.selected
                                )
                        )

                  else
                    selectedLabel

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
                        [ showOptions
                            { internalMsg = model.internalMsg
                            , selectedOptions = model.selected
                            , options = filterAndIndexOptions { options = options, selectedOptions = model.selected, searchFn = searchFn, searchText = model.searchText }
                            , optionLabelFn = optionLabelFn
                            , optionDescriptionFn = optionDescriptionFn
                            , optionsContainerMaxHeight = optionsContainerMaxHeight
                            , searchText = model.searchText
                            , focusedOptionIndex = model.focusedOptionIndex
                            , noResultsForMsg = noResultsForMsg
                            , noOptionsMsg = noOptionsMsg
                            }
                        ]

                  else
                    text ""
                ]
            ]
