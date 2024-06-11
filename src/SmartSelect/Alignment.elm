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

@docs style


# Test

@docs params

-}

import Browser.Dom as Dom exposing (Element)
import Html exposing (Html, div)
import Html.Attributes as Attrs
import SmartSelect.Id as Id
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


style : Maybe Alignment -> List (Html.Attribute msg)
style alignment =
    case alignment of
        Just (Alignment (Position { x, y, width }) placement) ->
            case placement of
                Above ->
                    [ Attrs.style "position" "fixed"
                    , Attrs.style "bottom" (String.fromFloat y ++ "px")
                    , Attrs.style "left" (String.fromFloat x ++ "px")
                    , Attrs.style "width" (String.fromFloat width ++ "px")
                    ]

                Below ->
                    [ Attrs.style "position" "fixed"
                    , Attrs.style "top" (String.fromFloat y ++ "px")
                    , Attrs.style "left" (String.fromFloat x ++ "px")
                    , Attrs.style "width" (String.fromFloat width ++ "px")
                    ]

        Nothing ->
            [ Attrs.style "position" "fixed"
            , Attrs.style "visibility" "hidden"
            ]


containerClass : Maybe Alignment -> String
containerClass alignment =
    case alignment of
        Just (Alignment _ Below) ->
            classPrefix "options-container-below"

        Just (Alignment _ Above) ->
            classPrefix "options-container-above"

        Nothing ->
            classPrefix "options-container-below"


view : Id.Prefix -> Maybe Alignment -> List (Html msg) -> Html msg
view prefix alignment children =
    div
        (Attrs.id (Id.container prefix)
            :: Attrs.class (classPrefix "container-wrapper")
            :: style alignment
        )
        [ div
            [ Attrs.class
                (String.join
                    " "
                    [ classPrefix "options-container"
                    , containerClass alignment
                    ]
                )
            ]
            children
        ]
