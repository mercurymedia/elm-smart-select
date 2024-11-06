module SmartSelect.Alignment exposing
    ( Alignment
    , init, getElements
    , style
    , getAlignment, view
    )

{-| Determine the Alignment for the select options container


# Definitions

@docs Alignment, Params


# Init

@docs init, getElements


# Style

@docs style, popoverWrapperStyles


# Test

@docs params

-}

import Browser exposing (element)
import Browser.Dom as Dom exposing (Element)
import Css exposing (block)
import Html.Styled exposing (div)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Json.Decode as Decode
import SmartSelect.Id as Id
import SmartSelect.Settings exposing (ScrollBehavior(..), Theme)
import SmartSelect.ViewComponents exposing (classPrefix)
import Task exposing (Task)
import Task.Extra as TaskExtra


type Alignment
    = Alignment Position Placement


type Position
    = Position { x : Float, y : Float, width : Float }


type Placement
    = Above
    | Below


init : { container : Element, select : Element, viewport : Dom.Viewport } -> Alignment
init { container, select, viewport } =
    if
        select.element.y
            + select.element.height
            + container.element.height
            >= viewport.viewport.height
    then
        Alignment
            (Position
                { x = select.element.x
                , y = viewport.viewport.height - select.element.y
                , width = select.element.width
                }
            )
            Above

    else
        Alignment
            (Position
                { x = select.element.x
                , y = select.element.y + select.element.height
                , width = select.element.width
                }
            )
            Below


getAlignment : Id.Prefix -> (Result Dom.Error Alignment -> msg) -> Cmd msg
getAlignment prefix handleResponse =
    Task.attempt handleResponse
        (getElements (Id.container prefix) (Id.select prefix))


getElements : String -> String -> Task Dom.Error Alignment
getElements containerId selectId =
    Task.succeed (\container select viewport -> init { container = container, select = select, viewport = viewport })
        |> TaskExtra.andMap (Dom.getElement containerId)
        |> TaskExtra.andMap (Dom.getElement selectId)
        |> TaskExtra.andMap Dom.getViewport



-- Test


style : Maybe Alignment -> { containerStyles : List Css.Style, elementStyles : List Css.Style }
style alignment =
    case alignment of
        Just (Alignment (Position { x, y, width }) placement) ->
            case placement of
                Above ->
                    { containerStyles = []
                    , elementStyles =
                        [ Css.bottom (Css.px y)
                        , Css.left (Css.px x)
                        , Css.width (Css.px width)
                        ]
                    }

                Below ->
                    { containerStyles = []
                    , elementStyles =
                        [ Css.top (Css.px y)
                        , Css.left (Css.px x)
                        , Css.width (Css.px width)
                        ]
                    }

        Nothing ->
            { containerStyles = [ Css.visibility Css.hidden ]
            , elementStyles = [ Css.visibility Css.hidden ]
            }


popoverWrapperStyles : Maybe Alignment -> List Css.Style
popoverWrapperStyles alignment =
    case alignment of
        Just (Alignment _ Below) ->
            [ Css.flexDirection Css.column ]

        Just (Alignment _ Above) ->
            [ Css.flexDirection Css.columnReverse ]

        Nothing ->
            [ Css.flexDirection Css.column ]


view : { theme : Theme, scrollBehavior : ScrollBehavior, idPrefix : Id.Prefix, onClose : msg, alignment : Maybe Alignment } -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
view { theme, scrollBehavior, idPrefix, onClose, alignment } children =
    let
        alignmentStyles =
            style alignment

        backdropAttrs =
            case scrollBehavior of
                BlockScrolling ->
                    [ Attrs.css [ Css.property "pointer-events" "all" ] ]

                CloseOnScroll ->
                    [ Attrs.css [ Css.property "pointer-events" "all" ]
                    , Events.stopPropagationOn "wheel" (Decode.succeed ( onClose, True ))
                    ]

                KeepOpen ->
                    [ Attrs.css [ Css.property "pointer-events" "none", Css.visibility Css.hidden ] ]
    in
    div
        [ Attrs.css
            [ Css.position Css.fixed
            , Css.left (Css.px 0)
            , Css.top (Css.px 0)
            , Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.zIndex (Css.int theme.zIndex)
            , Css.property "pointer-events" "none"
            , Css.batch alignmentStyles.containerStyles
            ]
        ]
        [ div
            (Attrs.css
                [ Css.position Css.absolute
                , Css.left (Css.px 0)
                , Css.top (Css.px 0)
                , Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                ]
                :: Events.onClick onClose
                :: backdropAttrs
            )
            []
        , div
            [ Attrs.id (Id.container idPrefix)
            , Attrs.css
                [ Css.padding2 (Css.rem 0.25) (Css.rem 0)
                , Css.position Css.absolute
                , Css.property "pointer-events" "all"
                , Css.batch alignmentStyles.elementStyles
                ]
            , Attrs.class (classPrefix theme.classNamePrefix "options-list-container")
            ]
            [ div
                [ Attrs.css
                    [ Css.backgroundColor theme.color.background.optionsContainer
                    , Css.zIndex (Css.int 50)
                    , Css.displayFlex
                    , Css.border3 theme.borderWidth Css.solid theme.color.border
                    , Css.borderRadius theme.borderRadius.base
                    , Css.boxShadow5
                        theme.boxShadow.offsetX
                        theme.boxShadow.offsetY
                        theme.boxShadow.blurRadius
                        theme.boxShadow.spreadRadius
                        theme.boxShadow.color
                    , Css.overflow Css.auto
                    , Css.batch (popoverWrapperStyles alignment)
                    ]
                , Attrs.class (classPrefix theme.classNamePrefix "options-list-container-inner")
                ]
                children
            ]
        ]
