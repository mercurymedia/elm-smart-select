module SmartSelect.ViewComponents exposing
    ( classPrefix
    , viewEmptyOptionsListItem
    , viewError
    , viewOptionsList
    , viewOptionsListItem
    , viewSearchPrompt
    , viewSearchPromptContainer
    , viewSpinner
    , viewTextField
    , viewTextFieldContainer
    )

import Color
import Css
import Css.Global
import Css.Transitions
import Html.Styled exposing (div, input, span, text)
import Html.Styled.Attributes exposing (attribute, class, css, disabled)
import Html.Styled.Events exposing (onClick)
import SmartSelect.Icons as Icons
import SmartSelect.Settings exposing (Theme)
import Spinner


styleList : List ( List Css.Style, Bool ) -> Css.Style
styleList list =
    list
        |> List.filter Tuple.second
        |> List.concatMap Tuple.first
        |> Css.batch


transitionAll : Theme -> Css.Style
transitionAll theme =
    Css.property "transition"
        ([ "all"
         , String.fromFloat theme.transition.duration ++ "ms"
         , "cubic-bezier(0.4, 0, 0.2, 1)"
         ]
            |> String.join " "
        )


colorsTransition : Theme -> Css.Style
colorsTransition theme =
    Css.Transitions.transition
        [ Css.Transitions.backgroundColor3 theme.transition.duration 0 (Css.Transitions.cubicBezier 0.4 0 0.2 1)
        , Css.Transitions.color3 theme.transition.duration 0 (Css.Transitions.cubicBezier 0.4 0 0.2 1)
        ]


classPrefix : String -> String -> String
classPrefix prefix className =
    prefix ++ "--" ++ className


viewTextFieldContainer : Theme -> List (Html.Styled.Attribute msg) -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewTextFieldContainer theme attrs children =
    div (class (classPrefix theme.classNamePrefix "text-field-container") :: attrs)
        children


viewOptionsList : Theme -> List (Html.Styled.Attribute msg) -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewOptionsList theme attrs children =
    div
        ([ css
            [ Css.width (Css.pct 100)
            , Css.overflow Css.auto
            , Css.color theme.color.text.primary
            ]
         , class (classPrefix theme.classNamePrefix "options-list")
         ]
            ++ attrs
        )
        children


viewOptionsListItem : Theme -> List (Html.Styled.Attribute msg) -> { label : String, description : String, isFocused : Bool, isSelected : Bool } -> Html.Styled.Html msg
viewOptionsListItem theme attrs { label, description, isFocused, isSelected } =
    let
        styles =
            styleList
                [ ( [ Css.backgroundColor theme.color.action.hover ]
                  , isFocused
                  )
                , ( [ Css.backgroundColor theme.color.primary.light
                    , Css.color theme.color.primary.main
                    ]
                  , isSelected
                  )
                ]
    in
    div
        ([ css
            [ Css.padding (Css.rem 0.5)
            , Css.cursor Css.pointer
            , colorsTransition theme
            , styles
            , Css.hover [ Css.backgroundColor theme.color.action.hover ]
            ]
         , class (classPrefix theme.classNamePrefix "options-list-item")
         ]
            ++ attrs
        )
        [ div [] [ text label ]
        , if description /= "" then
            div
                [ css
                    [ Css.fontSize theme.fontSize.xs
                    , Css.color theme.color.text.secondary
                    ]
                ]
                [ text description ]

          else
            text ""
        ]


viewEmptyOptionsListItem : Theme -> List (Html.Styled.Attribute msg) -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewEmptyOptionsListItem theme attrs children =
    div
        ([ css
            [ Css.width (Css.pct 100)
            , Css.padding (Css.rem 0.5)
            , Css.color theme.color.text.disabled
            , Css.fontStyle Css.italic
            ]
         , class (classPrefix theme.classNamePrefix "options-list-item-empty")
         ]
            ++ attrs
        )
        children


viewTextField :
    Theme
    -> List (Html.Styled.Attribute msg)
    ->
        { inputAttributes : List (Html.Styled.Attribute msg)
        , clearIconAttributes : Maybe (List (Html.Styled.Attribute msg))
        , selectedOptions : List (Html.Styled.Html msg)
        , isDisabled : Bool
        }
    -> Html.Styled.Html msg
viewTextField theme attrs { inputAttributes, selectedOptions, clearIconAttributes, isDisabled } =
    let
        disabledStyles =
            if isDisabled then
                [ Css.cursor Css.default
                , Css.opacity (Css.num 0.5)
                , Css.pointerEvents Css.none
                ]

            else
                []
    in
    div
        ([ css
            [ Css.displayFlex
            , Css.flexWrap Css.wrap
            , Css.alignItems Css.center
            , Css.property "gap" "0.25rem"
            , Css.borderRadius theme.borderRadius.base
            , Css.cursor Css.text_
            , Css.padding2 (Css.rem 0.5) (Css.rem 0.5)
            , Css.backgroundColor theme.color.background.input
            , Css.color theme.color.text.primary
            , Css.border3 theme.borderWidth Css.solid theme.color.border
            , Css.batch disabledStyles
            , transitionAll theme
            , Css.hover
                [ Css.backgroundColor theme.color.action.hover
                , Css.Global.descendants
                    [ Css.Global.selector "[data-action=clear]"
                        [ Css.opacity (Css.num 1) ]
                    ]
                ]
            , Css.pseudoClass "focus-within"
                [ Css.borderColor theme.color.primary.main
                , Css.backgroundColor theme.color.background.input
                ]
            ]
         , class (classPrefix theme.classNamePrefix "text-field")
         ]
            ++ attrs
        )
        (selectedOptions
            ++ [ div
                    [ css
                        [ Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.property "gap" "0.125rem"
                        , Css.flexGrow (Css.num 1)
                        ]
                    ]
                    [ viewInput theme (disabled isDisabled :: inputAttributes)
                    , case clearIconAttributes of
                        Just clearAttrs ->
                            viewIcon theme
                                ([ css [ Css.opacity (Css.num 0) ]
                                 , attribute "data-action" "clear"
                                 , class (classPrefix theme.classNamePrefix "clear-button")
                                 ]
                                    ++ clearAttrs
                                )
                                Icons.x

                        Nothing ->
                            text ""
                    , viewIcon theme
                        [ class (classPrefix theme.classNamePrefix "chevron-button") ]
                        Icons.chevronDown
                    ]
               ]
        )


viewInput : Theme -> List (Html.Styled.Attribute msg) -> Html.Styled.Html msg
viewInput theme attrs =
    input
        ([ css
            [ Css.flexGrow (Css.num 1)
            , Css.outline Css.none
            , Css.borderWidth (Css.px 0)
            , Css.borderRadius (Css.px 0)
            , Css.backgroundColor Css.transparent
            , Css.width (Css.px 0)
            , Css.minWidth (Css.px 40)
            , Css.minHeight theme.size.inputElement
            , Css.pseudoClass ":placeholder"
                [ Css.textOverflow Css.ellipsis
                , Css.color theme.color.text.disabled
                ]
            , Css.pseudoClass "placeholder-shown"
                [ Css.textOverflow Css.ellipsis
                , Css.color theme.color.text.disabled
                ]
            ]
         , class (classPrefix theme.classNamePrefix "input")
         ]
            ++ attrs
        )
        []


viewIcon : Theme -> List (Html.Styled.Attribute msg) -> Icons.Icon -> Html.Styled.Html msg
viewIcon theme attrs icon =
    span
        ([ css
            [ Css.width theme.size.iconButton
            , Css.height theme.size.iconButton
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.borderRadius (Css.pct 50)
            , Css.flexShrink (Css.num 0)
            , Css.cursor Css.pointer
            , Css.color theme.color.text.secondary
            , transitionAll theme
            , Css.hover [ Css.backgroundColor theme.color.action.hover ]
            ]
         , class (classPrefix theme.classNamePrefix "icon")
         ]
            ++ attrs
        )
        [ icon
            |> Icons.withSize 16
            |> Icons.withStrokeWidth 2
            |> Icons.toHtml []
            |> Html.Styled.fromUnstyled
        ]


viewError : Theme -> List (Html.Styled.Attribute msg) -> { message : String, onDismiss : msg } -> Html.Styled.Html msg
viewError theme attrs { message, onDismiss } =
    div
        ([ css [ Css.padding (Css.rem 0.5) ]
         , class (classPrefix theme.classNamePrefix "error-container")
         ]
            ++ attrs
        )
        [ div
            [ css
                [ Css.width (Css.pct 100)
                , Css.padding (Css.rem 0.5)
                , Css.backgroundColor theme.color.error.light
                , Css.color theme.color.error.main
                , Css.borderRadius theme.borderRadius.base
                , Css.displayFlex
                , Css.justifyContent Css.spaceBetween
                , Css.alignItems Css.center
                ]
            , class (classPrefix theme.classNamePrefix "error-container-inner")
            ]
            [ div [ css [ Css.flexWrap Css.wrap ], class (classPrefix theme.classNamePrefix "error-message") ]
                [ text message ]
            , span
                [ css [ Css.cursor Css.pointer ]
                , onClick onDismiss
                , class (classPrefix theme.classNamePrefix "error-close-button")
                ]
                [ Icons.x
                    |> Icons.withSize 12
                    |> Icons.withStrokeWidth 4
                    |> Icons.toHtml []
                    |> Html.Styled.fromUnstyled
                ]
            ]
        ]


viewSearchPromptContainer : Theme -> List (Html.Styled.Attribute msg) -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewSearchPromptContainer theme attrs children =
    div
        ([ css
            [ Css.width (Css.pct 100)
            , Css.color theme.color.text.disabled
            , Css.fontStyle Css.italic
            ]
         , class (classPrefix theme.classNamePrefix "search-prompt-container")
         ]
            ++ attrs
        )
        children


viewSearchPrompt : Theme -> List (Html.Styled.Attribute msg) -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewSearchPrompt theme attrs children =
    div
        ([ css
            [ Css.width (Css.pct 100)
            , Css.padding (Css.rem 0.5)
            ]
         , class (classPrefix theme.classNamePrefix "search-prompt")
         ]
            ++ attrs
        )
        children


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


viewSpinner : Theme -> { spinner : Spinner.Model, spinnerColor : Color.Color } -> Html.Styled.Html msg
viewSpinner theme { spinner, spinnerColor } =
    div
        [ css
            [ Css.width (Css.pct 100)
            , Css.padding (Css.rem 1)
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            , Css.height (Css.rem 4)
            ]
        , class (classPrefix theme.classNamePrefix "spinner-container")
        ]
        [ div [ css [ Css.position Css.relative ], class (classPrefix theme.classNamePrefix "spinner") ]
            [ Html.Styled.fromUnstyled <|
                Spinner.view (spinnerConfig spinnerColor) spinner
            ]
        ]
