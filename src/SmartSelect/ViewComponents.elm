module SmartSelect.ViewComponents exposing
    ( classPrefix
    , viewEmptyOptionsListItem
    , viewError
    , viewOptionsList
    , viewOptionsListItem
    , viewSpinner
    , viewTextField
    , viewTextFieldContainer
    )

import Color
import Html.Styled exposing (div, input, span, text)
import Html.Styled.Attributes exposing (class, classList, disabled)
import Html.Styled.Events exposing (onClick)
import SmartSelect.Icons as Icons
import Spinner


classPrefix : String -> String
classPrefix class =
    "elm-smart-select--" ++ class


viewTextFieldContainer : List (Html.Styled.Attribute msg) -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewTextFieldContainer attrs children =
    div attrs
        children


viewOptionsList : List (Html.Styled.Attribute msg) -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewOptionsList attrs children =
    div (class (classPrefix "options-list") :: attrs)
        children


viewOptionsListItem : List (Html.Styled.Attribute msg) -> { label : String, description : String, isFocused : Bool, isSelected : Bool } -> Html.Styled.Html msg
viewOptionsListItem attrs { label, description, isFocused, isSelected } =
    div
        (classList
            [ ( classPrefix "options-list-item", True )
            , ( classPrefix "options-list-item-focused", isFocused )
            , ( classPrefix "options-list-item-selected", isSelected )
            ]
            :: attrs
        )
        [ div [] [ text label ]
        , if description /= "" then
            div
                [ classList
                    [ ( classPrefix "options-list-item-description", True )
                    , ( classPrefix "options-list-item-description-unfocused", not isFocused )
                    , ( classPrefix "options-list-item-description-focused", isFocused )
                    ]
                ]
                [ text description ]

          else
            text ""
        ]


viewEmptyOptionsListItem : List (Html.Styled.Attribute msg) -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewEmptyOptionsListItem attrs children =
    div (class (classPrefix "search-or-no-results-text") :: attrs) children


viewTextField :
    List (Html.Styled.Attribute msg)
    ->
        { inputAttributes : List (Html.Styled.Attribute msg)
        , clearIconAttributes : Maybe (List (Html.Styled.Attribute msg))
        , selectedOptions : List (Html.Styled.Html msg)
        , isDisabled : Bool
        }
    -> Html.Styled.Html msg
viewTextField attrs { inputAttributes, selectedOptions, clearIconAttributes, isDisabled } =
    div
        (classList
            [ ( classPrefix "text-field", True )
            , ( classPrefix "disabled", isDisabled )
            ]
            :: attrs
        )
        (selectedOptions
            ++ [ div [ class (classPrefix "text-field-input-container") ]
                    [ input (disabled isDisabled :: inputAttributes) []
                    , case clearIconAttributes of
                        Just clearAttrs ->
                            viewIcon (class (classPrefix "text-field-icon-x") :: clearAttrs) Icons.x

                        Nothing ->
                            text ""
                    , viewIcon [ class (classPrefix "text-field-icon-chevron") ] Icons.chevronDown
                    ]
               ]
        )


viewIcon : List (Html.Styled.Attribute msg) -> Icons.Icon -> Html.Styled.Html msg
viewIcon attrs icon =
    span (class (classPrefix "text-field-icon") :: attrs)
        [ icon
            |> Icons.withSize 16
            |> Icons.withStrokeWidth 2
            |> Icons.toHtml []
            |> Html.Styled.fromUnstyled
        ]


viewError : List (Html.Styled.Attribute msg) -> { message : String, onDismiss : msg } -> Html.Styled.Html msg
viewError attrs { message, onDismiss } =
    div (class (classPrefix "error-box-container") :: attrs)
        [ div [ class (classPrefix "error-box") ]
            [ div [ class (classPrefix "error-container") ]
                [ text message ]
            , span
                [ class (classPrefix "dismiss-error-x")
                , onClick onDismiss
                ]
                [ Icons.x
                    |> Icons.withSize 12
                    |> Icons.withStrokeWidth 4
                    |> Icons.toHtml []
                    |> Html.Styled.fromUnstyled
                ]
            ]
        ]


spinnerConfig : Color.Color -> Spinner.Config
spinnerConfig color =
    { lines = 10
    , length = 5
    , width = 3
    , radius = 8
    , scale = 0.75
    , corners = 1
    , opacity = 0.5
    , rotate = 0
    , direction = Spinner.Clockwise
    , speed = 1
    , trail = 60
    , translateX = 50
    , translateY = 50
    , shadow = False
    , hwaccel = True
    , color = always color
    }


viewSpinner : { spinner : Spinner.Model, spinnerColor : Color.Color } -> Html.Styled.Html msg
viewSpinner { spinner, spinnerColor } =
    div [ class (classPrefix "loading-spinner-container") ]
        [ div [ class (classPrefix "loading-spinner") ]
            [ Html.Styled.fromUnstyled <|
                Spinner.view (spinnerConfig spinnerColor) spinner
            ]
        ]
