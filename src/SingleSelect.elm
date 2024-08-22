module SingleSelect exposing (SmartSelect, Msg, init, view, viewCustom, subscriptions, update, updatePosition)

{-| A select component for a single selection with local data.


# Architecture

@docs SmartSelect, Msg, init, view, viewCustom, subscriptions, update, updatePosition

-}

import Browser.Dom as Dom
import Browser.Events
import Dict
import Html exposing (Html)
import Html.Styled exposing (text)
import Html.Styled.Attributes exposing (autocomplete, classList, id, placeholder, style, value)
import Html.Styled.Events as Events exposing (onClick, onInput, onMouseEnter)
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..))
import SmartSelect.Alignment as Alignment exposing (Alignment)
import SmartSelect.Id as Id exposing (Prefix(..))
import SmartSelect.Utilities as Utilities exposing (KeyCode(..))
import SmartSelect.ViewComponents
    exposing
        ( classPrefix
        , viewEmptyOptionsListItem
        , viewOptionsList
        , viewOptionsListItem
        , viewTextField
        , viewTextFieldContainer
        )


{-| The opaque type representing a particular smart select instance.
-}
type SmartSelect msg a
    = SmartSelect (Model msg a)


type alias Model msg a =
    { isOpen : Bool
    , searchText : String
    , focusedOptionIndex : Int
    , selectionMsg : ( a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , alignment : Maybe Alignment
    , idPrefix : Prefix
    }


{-| Opaque type representing cases to be passed to SingleSelect.update
-}
type Msg a
    = NoOp
    | SetFocused Int
    | UpKeyPressed Int
    | DownKeyPressed Int
    | SetSearchText String
    | WindowResized ( Int, Int )
    | GotAlignment (Result Dom.Error Alignment)
    | Open
    | Close


{-| Instantiates and returns a smart select.

  - `selectionMsg` takes a function that expects a tuple representing the selection and a SinglSelect.Msg msg and returns an externally defined msg for handling selection.
  - `internalMsg` takes a function that expects a SinglSelect.Msg and returns an externally defined msg for handling the update of the select.
  - `idPrefix` takes a string with a unique prefix

-}
init : { selectionMsg : ( a, Msg a ) -> msg, internalMsg : Msg a -> msg, idPrefix : String } -> SmartSelect msg a
init { selectionMsg, internalMsg, idPrefix } =
    SmartSelect
        { isOpen = False
        , searchText = ""
        , focusedOptionIndex = 0
        , selectionMsg = selectionMsg
        , internalMsg = internalMsg
        , alignment = Nothing
        , idPrefix = Prefix idPrefix
        }


{-| Events external to the smart select to which it is subscribed.
-}
subscriptions : SmartSelect msg a -> Sub msg
subscriptions (SmartSelect model) =
    if model.isOpen then
        Sub.batch
            [ Browser.Events.onResize (\h w -> model.internalMsg <| WindowResized ( h, w ))
            , Browser.Events.onMouseDown (Utilities.clickedOutsideSelect (Id.select model.idPrefix) (model.internalMsg Close))
            ]

    else
        Sub.none


keyActionMapper : { options : List ( Int, a ), focusedOptionIndex : Int, selectionMsg : ( a, Msg a ) -> msg, internalMsg : Msg a -> msg } -> Decode.Decoder ( msg, Bool )
keyActionMapper { options, focusedOptionIndex, selectionMsg, internalMsg } =
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


{-| Update the provided smart select and receive the updated select instance and a cmd to run.
-}
update : Msg a -> SmartSelect msg a -> ( SmartSelect msg a, Cmd msg )
update msg (SmartSelect model) =
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
            ( SmartSelect { model | searchText = text, focusedOptionIndex = 0 }, Cmd.none )

        WindowResized _ ->
            ( SmartSelect model, Alignment.getAlignment model.idPrefix (\alignment -> model.internalMsg (GotAlignment alignment)) )

        GotAlignment result ->
            case result of
                Ok alignment ->
                    ( SmartSelect { model | alignment = Just alignment }, Cmd.none )

                Err _ ->
                    ( SmartSelect model, Cmd.none )

        Open ->
            if model.isOpen then
                ( SmartSelect model, Cmd.none )

            else
                openPopover (SmartSelect model)

        Close ->
            ( SmartSelect { model | isOpen = False, searchText = "", alignment = Nothing }
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
    , options : List ( Int, a )
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    , searchText : String
    , focusedOptionIndex : Int
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    , idPrefix : Prefix
    }
    -> Html.Styled.Html msg
showOptions { selectionMsg, selectedOption, internalMsg, options, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight, searchText, focusedOptionIndex, noResultsForMsg, noOptionsMsg, idPrefix } =
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
                        [ Events.stopPropagationOn "click" (Decode.succeed ( selectionMsg ( option, Close ), True ))
                        , onMouseEnter <| internalMsg <| SetFocused idx
                        , id (Id.option idPrefix idx)
                        ]
                        { label = optionLabelFn option
                        , description = optionDescriptionFn option
                        , isFocused = idx == focusedOptionIndex
                        , isSelected = isSelected
                        }
                )
                options
        )


indexOptions : { options : List a, searchFn : String -> List a -> List a, searchText : String } -> List ( Int, a )
indexOptions { options, searchFn, searchText } =
    searchFn searchText options
        |> List.indexedMap Tuple.pair


{-| The smart select view for selecting one option at a time with local data.

  - `selected` takes the currently selected entity, if any.
  - `options` takes a list of the data being selected from.
  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".

-}
view : { selected : Maybe a, options : List a, optionLabelFn : a -> String } -> SmartSelect msg a -> Html msg
view config smartSelect =
    viewStyled config smartSelect
        |> Html.Styled.toUnstyled


viewStyled : { selected : Maybe a, options : List a, optionLabelFn : a -> String } -> SmartSelect msg a -> Html.Styled.Html msg
viewStyled { selected, options, optionLabelFn } smartSelect =
    let
        config =
            { isDisabled = False
            , selected = selected
            , options = options
            , optionLabelFn = optionLabelFn
            , optionDescriptionFn = \_ -> ""
            , optionsContainerMaxHeight = 300
            , searchFn =
                \searchText allOptions ->
                    List.filter (\option -> String.contains (String.toLower searchText) (String.toLower <| optionLabelFn option)) allOptions
            , searchPrompt = "Placeholder..."
            , noResultsForMsg = \_ -> "No results"
            , noOptionsMsg = ""
            }
    in
    viewCustomStyled config smartSelect


{-| The customizable smart select view for selecting one option at a time with local data.

  - `isDisabled` takes a boolean that indicates whether or not the select can be opened.
  - `selected` takes the currently selected entity, if any.
  - `options` takes a list of the data being selected from.
  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".
  - `optionDescriptionFn` takes a function that expects an instance of the data being selected from and returns a string describing the instance, i.e. if the label is "Garden Hose", the description may be "30 ft".
  - `optionsContainerMaxHeight` takes a float that specifies the max height of the container of the selectable options.
  - `searchFn` takes a function that expects the search text and the items to search and returns the filtered items.
  - `selectTitle` takes a string to label the select in its closed state and non-selected state.
  - `searchPrompt` takes a string to indicate what is being searched for.
  - `noResultsForMsg` takes a function that expects a string and returns a message indicating that the search for the provided string returned no results.
  - `noOptionsMsg` takes a string to indicate that no options exist in the select.

```elm
import Html exposing (Html)
import SingleSelect

type Msg
    = HandleSelectUpdate (SingleSelect.Msg Product)
    | HandleSelection ( Product, SingleSelect.Msg Product )

type alias Product =
    { name : String
    , description : String
    , price : Float
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { products = exampleProducts
      , select =
            SingleSelect.init
                { selectionMsg = HandleSelection
                , internalMsg = HandleSelectUpdate
                }
      , selectedProduct = Nothing
      }
    , Cmd.none
    )

type alias Model =
    { products : List Product
    , select : SingleSelect.SmartSelect Msg Product
    , selectedProduct : Maybe Product
    }

viewCustomProductSelect : Model -> Html Msg
viewCustomProductSelect model =
    SingleSelect.viewCustom
        { isDisabled = False
        , selected = model.selectedProduct
        , options = model.products
        , optionLabelFn = .name
        , optionDescriptionFn = \option -> "$" ++ String.fromFloat option.price
        , optionsContainerMaxHeight = 500
        , searchFn =
            \searchText allOptions ->
                List.filter
                    (\option ->
                        String.contains (String.toLower searchText) (String.toLower option.name)
                            || String.contains (String.toLower searchText) (String.toLower option.description)
                    )
                    allOptions
        , selectTitle = "Select a Product"
        , searchPrompt = "Search for a Product"
        , noResultsForMsg = \searchText -> "No results found for: " ++ searchText
        , noOptionsMsg = "There are no options to select"
        }
        model.select
```

-}
viewCustom :
    { isDisabled : Bool
    , selected : Maybe a
    , options : List a
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    , searchFn : String -> List a -> List a
    , searchPrompt : String
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    }
    -> SmartSelect msg a
    -> Html msg
viewCustom config smartSelect =
    viewCustomStyled config smartSelect
        |> Html.Styled.toUnstyled


viewCustomStyled :
    { isDisabled : Bool
    , selected : Maybe a
    , options : List a
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    , searchFn : String -> List a -> List a
    , searchPrompt : String
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    }
    -> SmartSelect msg a
    -> Html.Styled.Html msg
viewCustomStyled { isDisabled, selected, options, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight, searchFn, searchPrompt, noResultsForMsg, noOptionsMsg } (SmartSelect model) =
    let
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
                { options = indexOptions { options = options, searchFn = searchFn, searchText = model.searchText }
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
                , onInput <| \newValue -> model.internalMsg <| SetSearchText newValue
                , placeholder <| searchPrompt
                , value inputValue
                ]
            , isDisabled = isDisabled
            , selectedOptions = []
            , clearIconAttributes = Nothing
            }
        , Alignment.view
            model.idPrefix
            model.alignment
            [ showOptions
                { selectionMsg = model.selectionMsg
                , selectedOption = selected
                , internalMsg = model.internalMsg
                , options = indexOptions { options = options, searchFn = searchFn, searchText = model.searchText }
                , optionLabelFn = optionLabelFn
                , optionDescriptionFn = optionDescriptionFn
                , optionsContainerMaxHeight = optionsContainerMaxHeight
                , searchText = model.searchText
                , focusedOptionIndex = model.focusedOptionIndex
                , noResultsForMsg = noResultsForMsg
                , noOptionsMsg = noOptionsMsg
                , idPrefix = model.idPrefix
                }
            ]
        ]
