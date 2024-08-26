module MultiSelect exposing (SmartSelect, Msg, init, view, viewCustom, subscriptions, update, updatePosition)

{-| A select component for multi selection with local data.


# Architecture

@docs SmartSelect, Msg, init, view, viewCustom, subscriptions, update, updatePosition

-}

import Browser.Dom as Dom
import Browser.Events
import Dict
import Html exposing (Html)
import Html.Styled exposing (div, text)
import Html.Styled.Attributes exposing (autocomplete, classList, id, placeholder, style, value)
import Html.Styled.Events as Events exposing (onClick, onInput, onMouseEnter)
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..))
import SmartSelect.Alignment as Alignment exposing (Alignment)
import SmartSelect.Id as Id exposing (Prefix(..))
import SmartSelect.Settings exposing (Settings, defaultSettings, defaultTheme)
import SmartSelect.Utilities as Utilities exposing (KeyCode(..))
import SmartSelect.ViewComponents exposing (viewEmptyOptionsListItem, viewOptionsList, viewOptionsListItem, viewTextField, viewTextFieldContainer)


{-| The opaque type representing a particular smart select instance.
-}
type SmartSelect msg a
    = SmartSelect (Model msg a)


type alias Model msg a =
    { isOpen : Bool
    , searchText : String
    , focusedOptionIndex : Int
    , selectionMsg : ( List a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , alignment : Maybe Alignment
    , idPrefix : Prefix
    }


{-| Opaque type representing cases to be passed to MultiSelect.update
-}
type Msg a
    = NoOp
    | SetFocused Int
    | UpKeyPressed Int
    | DownKeyPressed Int
    | SetSearchText String
    | WindowResized ( Int, Int )
    | GotAlignment (Result Dom.Error Alignment)
    | SelectionChanged (Maybe Int)
    | Open
    | Close
    | Clear


{-| Instantiates and returns a smart select.

  - `selectionMsg` takes a function that expects a tuple representing the list of selections and a MultiSelect.Msg and returns an externally defined msg for handling selection.
  - `internalMsg` takes a function that expects a MultiSelect.Msg and returns an externally defined msg for handling the update of the select.
  - `idPrefix` takes a string with a unique prefix

-}
init : { selectionMsg : ( List a, Msg a ) -> msg, internalMsg : Msg a -> msg, idPrefix : String } -> SmartSelect msg a
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


keyActionMapper : { options : List ( Int, a ), selectedOptions : List a, focusedOptionIndex : Int, selectionMsg : ( List a, Msg a ) -> msg, internalMsg : Msg a -> msg, searchText : String } -> Decode.Decoder ( msg, Bool )
keyActionMapper { options, selectedOptions, focusedOptionIndex, selectionMsg, internalMsg, searchText } =
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


{-| Update the provided smart select and receive the updated select instance and a cmd to run.
-}
update : Msg a -> SmartSelect msg a -> ( SmartSelect msg a, Cmd msg )
update msg (SmartSelect model) =
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
            ( SmartSelect { model | searchText = text, focusedOptionIndex = 0 }, Cmd.none )

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

        Open ->
            if model.isOpen then
                ( SmartSelect model, Cmd.none )

            else
                ( SmartSelect { model | isOpen = True, focusedOptionIndex = 0 }
                , Cmd.batch
                    [ Alignment.getAlignment model.idPrefix (\alignment -> model.internalMsg (GotAlignment alignment))
                    , Utilities.focusInput model.idPrefix (model.internalMsg NoOp)
                    ]
                )

        Close ->
            if model.isOpen then
                ( SmartSelect { model | isOpen = False, searchText = "", alignment = Nothing }, Utilities.blurInput model.idPrefix (model.internalMsg NoOp) )

            else
                ( SmartSelect model, Cmd.none )

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
            ( SmartSelect { model | searchText = "" }, cmd )


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
    { selectionMsg : ( List a, Msg a ) -> msg
    , internalMsg : Msg a -> msg
    , selectedOptions : List a
    , options : List ( Int, a )
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , optionsContainerMaxHeight : Float
    , searchText : String
    , focusedOptionIndex : Int
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    , idPrefix : Prefix
    , settings : Settings
    }
    -> Html.Styled.Html msg
showOptions { selectionMsg, internalMsg, selectedOptions, options, optionLabelFn, optionDescriptionFn, optionsContainerMaxHeight, searchText, focusedOptionIndex, noResultsForMsg, noOptionsMsg, idPrefix, settings } =
    viewOptionsList settings.theme
        [ style "max-height" (String.fromFloat optionsContainerMaxHeight ++ "px") ]
        (if List.isEmpty options && searchText /= "" then
            [ viewEmptyOptionsListItem settings.theme [] [ text <| noResultsForMsg searchText ] ]

         else if List.isEmpty options then
            [ viewEmptyOptionsListItem settings.theme [] [ text noOptionsMsg ] ]

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


{-| The smart select view for selecting multiple options at a time with local data.

  - `selected` takes a list of the currently selected entities.
  - `options` takes a list of the data being selected from.
  - `optionLabelFn` takes a function that expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose".
  - `viewSelectedOptionFn` takes a function that expects and instance of the data being selected from and returns html to render a selected option.

-}
view :
    { selected : List a
    , options : List a
    , optionLabelFn : a -> String
    , viewSelectedOptionFn : a -> Html msg
    , settings : Settings
    }
    -> SmartSelect msg a
    -> Html msg
view config smartSelect =
    viewStyled config smartSelect
        |> Html.Styled.toUnstyled


viewStyled :
    { selected : List a
    , options : List a
    , optionLabelFn : a -> String
    , viewSelectedOptionFn : a -> Html msg
    , settings : Settings
    }
    -> SmartSelect msg a
    -> Html.Styled.Html msg
viewStyled { selected, options, optionLabelFn, viewSelectedOptionFn, settings } smartSelect =
    let
        config =
            { isDisabled = False
            , selected = selected
            , options = options
            , optionLabelFn = optionLabelFn
            , optionDescriptionFn = \_ -> ""
            , viewSelectedOptionFn = viewSelectedOptionFn
            , optionsContainerMaxHeight = 300
            , searchFn =
                \searchText allOptions ->
                    List.filter (\option -> String.contains (String.toLower searchText) (String.toLower <| optionLabelFn option)) allOptions
            , selectTitle = "Placeholder..."
            , noResultsForMsg = \_ -> "No results found"
            , noOptionsMsg = "No options available"
            , settings = settings
            }
    in
    viewCustomStyled config smartSelect


{-| The smart select view for selecting multiple options at a time with local data.

  - `isDisabled` takes a boolean that indicates whether or not the select can be opened.
  - `selected` takes a list of the currently selected entities.
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
import Html exposing (Html, div)
import MultiSelect

type Msg
    = HandleSelectUpdate (MultiSelect.Msg Product)
    | HandleSelection ( List Product, MultiSelect.Msg Product )

type alias Product =
    { name : String
    , description : String
    , price : Float
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { products = exampleProducts
      , select =
            MultiSelect.init
                { selectionMsg = HandleSelection
                , internalMsg = HandleSelectUpdate
                }
      , selectedProduct = Nothing
      }
    , Cmd.none
    )

type alias Model =
    { products : List Product
    , select : MultiSelect.SmartSelect Msg Product
    , selectedProducts : List Product
    }

viewSelectedProduct : Product -> Html Msg
viewSelectedProduct product =
    div []
        [ text (product.name ++ " - " ++ ("$" ++ String.fromFloat product.price)) ]

viewCustomProductSelect : Model -> Html Msg
viewCustomProductSelect model =
    MultiSelect.viewCustom
        { isDisabled = False
        , selected = model.selectedProducts
        , options = model.products
        , optionLabelFn = .name
        , optionDescriptionFn = \option -> "$" ++ String.fromFloat option.price
        , viewSelectedOptionFn = viewSelectedProduct
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
        , noResultsForMsg = \searchText -> "No results found for: " ++ searchText
        , noOptionsMsg = "There are no options to select"
        }
        model.select
```

-}
viewCustom :
    { isDisabled : Bool
    , selected : List a
    , options : List a
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , viewSelectedOptionFn : a -> Html msg
    , optionsContainerMaxHeight : Float
    , searchFn : String -> List a -> List a
    , selectTitle : String
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    , settings : Settings
    }
    -> SmartSelect msg a
    -> Html msg
viewCustom config smartSelect =
    viewCustomStyled config smartSelect
        |> Html.Styled.toUnstyled


viewCustomStyled :
    { isDisabled : Bool
    , selected : List a
    , options : List a
    , optionLabelFn : a -> String
    , optionDescriptionFn : a -> String
    , viewSelectedOptionFn : a -> Html msg
    , optionsContainerMaxHeight : Float
    , searchFn : String -> List a -> List a
    , selectTitle : String
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    , settings : Settings
    }
    -> SmartSelect msg a
    -> Html.Styled.Html msg
viewCustomStyled { isDisabled, selected, options, optionLabelFn, optionDescriptionFn, viewSelectedOptionFn, optionsContainerMaxHeight, searchFn, selectTitle, noResultsForMsg, noOptionsMsg, settings } (SmartSelect model) =
    viewTextFieldContainer settings.theme
        [ id (Id.select model.idPrefix)
        , classList
            [ ( "enabled-closed", not model.isOpen )
            , ( "enabled-opened", model.isOpen )
            ]
        , Events.stopPropagationOn "keypress" (Decode.map Utilities.alwaysStopPropogation (Decode.succeed <| model.internalMsg NoOp))
        , Events.preventDefaultOn "keydown"
            (keyActionMapper
                { options = filterAndIndexOptions { options = options, selectedOptions = selected, searchFn = searchFn, searchText = model.searchText }
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
            , clearIconAttributes = Just [ Events.stopPropagationOn "click" (Decode.succeed ( model.selectionMsg ( [], Clear ), True )) ]
            }
        , Alignment.view
            settings.theme
            model.idPrefix
            model.alignment
            [ showOptions
                { selectionMsg = model.selectionMsg
                , internalMsg = model.internalMsg
                , selectedOptions = selected
                , options = filterAndIndexOptions { options = options, selectedOptions = selected, searchFn = searchFn, searchText = model.searchText }
                , optionLabelFn = optionLabelFn
                , optionDescriptionFn = optionDescriptionFn
                , optionsContainerMaxHeight = optionsContainerMaxHeight
                , searchText = model.searchText
                , focusedOptionIndex = model.focusedOptionIndex
                , noResultsForMsg = noResultsForMsg
                , noOptionsMsg = noOptionsMsg
                , idPrefix = model.idPrefix
                , settings = settings
                }
            ]
        ]
