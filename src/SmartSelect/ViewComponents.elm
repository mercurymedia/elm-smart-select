module SmartSelect.ViewComponents exposing
    ( viewEmptyOptionsListItem
    , viewError
    , viewOptionsList
    , viewOptionsListItem
    , viewTextField
    , viewTextFieldContainer
    )

import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import SmartSelect.Icons as Icons
import SmartSelect.Utilities exposing (classPrefix)


viewTextFieldContainer : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewTextFieldContainer attrs children =
    div attrs
        children


viewOptionsList : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewOptionsList attrs children =
    div (class (classPrefix "options-list") :: attrs)
        children


viewOptionsListItem : List (Html.Attribute msg) -> { label : String, description : String, isFocused : Bool, isSelected : Bool } -> Html msg
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


viewEmptyOptionsListItem : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewEmptyOptionsListItem attrs children =
    div (class (classPrefix "search-or-no-results-text") :: attrs) children


viewTextField :
    List (Html.Attribute msg)
    ->
        { inputAttributes : List (Html.Attribute msg)
        , clearIconAttributes : List (Html.Attribute msg)
        , selectedOptions : List (Html msg)
        , isDisabled : Bool
        }
    -> Html msg
viewTextField attrs { inputAttributes, selectedOptions, clearIconAttributes, isDisabled } =
    div
        (classList
            [ ( classPrefix "text-field", True )
            , ( classPrefix "disabled", isDisabled )
            ]
            :: attrs
        )
        (selectedOptions
            ++ [ input (disabled isDisabled :: inputAttributes) []
               , div [ class (classPrefix "text-field__icons") ]
                    [ viewIcon (class (classPrefix "text-field__icon_x") :: clearIconAttributes) Icons.x
                    , viewIcon [ class (classPrefix "text-field__icon_chevron") ] Icons.chevronDown
                    ]
               ]
        )


viewIcon : List (Html.Attribute msg) -> Icons.Icon -> Html msg
viewIcon attrs icon =
    span (class (classPrefix "text-field__icon") :: attrs)
        [ icon
            |> Icons.withSize 16
            |> Icons.withStrokeWidth 2
            |> Icons.toHtml []
        ]


viewError : List (Html.Attribute msg) -> { message : String, onDismiss : msg } -> Html msg
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
                ]
            ]
        ]
