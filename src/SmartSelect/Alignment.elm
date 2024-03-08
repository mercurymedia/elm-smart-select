module SmartSelect.Alignment exposing
    ( Alignment, Params
    , init, getElements
    , style
    , params
    , view
    )

{-| Determine the Alignment for the select options container


# Definitions

@docs Alignment, Params


# Init

@docs init, getElements


# Style

@docs style


# Test

@docs params

-}

import Browser.Dom as Dom exposing (Element)
import Html exposing (Html, div)
import Html.Attributes as Attrs
import Task exposing (Task)
import Task.Extra as TaskExtra


type Alignment
    = Alignment Position Placement


type Position
    = Position { x : Float, y : Float, width : Float }


type Placement
    = Above
    | Below


type Params
    = Params { container : Element, select : Element, viewport : Dom.Viewport }


init : Params -> Alignment
init (Params { container, select, viewport }) =
    if
        select.element.y
            + select.element.height
            + container.element.height
            >= viewport.viewport.height
    then
        Alignment
            (Position
                { x = select.element.x
                , y = select.element.y - container.element.height
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


getElements : String -> String -> Task Dom.Error Params
getElements containerId selectId =
    Task.succeed (\container select viewport -> Params { container = container, select = select, viewport = viewport })
        |> TaskExtra.andMap (Dom.getElement containerId)
        |> TaskExtra.andMap (Dom.getElement selectId)
        |> TaskExtra.andMap Dom.getViewport



-- Test


params : { container : Element, select : Element, viewport : Dom.Viewport } -> Params
params =
    Params


style : Maybe Alignment -> List (Html.Attribute msg)
style alignment =
    case alignment of
        Just (Alignment (Position { x, y, width }) placement) ->
            [ Attrs.style "position" "fixed"
            , Attrs.style "top" (String.fromFloat y ++ "px")
            , Attrs.style "left" (String.fromFloat x ++ "px")
            , Attrs.style "width" (String.fromFloat width ++ "px")
            ]

        Nothing ->
            [ Attrs.style "position" "fixed"
            , Attrs.style "visibility" "hidden"
            ]


containerClass : String -> Maybe Alignment -> String
containerClass classPrefix alignment =
    case alignment of
        Just (Alignment _ Below) ->
            classPrefix ++ "options-container-below"

        Just (Alignment _ Above) ->
            classPrefix ++ "options-container-above"

        Nothing ->
            ""


view : String -> Maybe Alignment -> List (Html msg) -> Html msg
view classPrefix alignment children =
    div
        (Attrs.id (classPrefix ++ "container")
            :: Attrs.class (classPrefix ++ "container-wrapper")
            :: style alignment
        )
        [ div
            [ Attrs.class
                (String.join
                    " "
                    [ classPrefix ++ "options-container"
                    , containerClass classPrefix alignment
                    ]
                )
            ]
            children
        ]
