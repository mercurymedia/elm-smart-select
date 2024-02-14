module SmartSelect.Alignment exposing
    ( Alignment, Params
    , init, getElements
    , containerClass, selectClass
    , isAbove, params
    )

{-| Determine the Alignment for the select options container


# Definitions

@docs Alignment, Params


# Init

@docs init, getElements


# Classes

@docs containerClass, selectClass


# Test

@docs isAbove params

-}

import Browser.Dom as Dom exposing (Element)
import Task exposing (Task)
import Task.Extra as TaskExtra


type Alignment
    = Above
    | Below


type Params
    = Params { container : Element, select : Element, viewport : Dom.Viewport }


init : Params -> Alignment
init (Params { container, select, viewport }) =
    if
        select.viewport.y
            + select.element.y
            + select.element.height
            + container.element.height
            >= viewport.viewport.height
    then
        Above

    else
        Below


containerClass : String -> Maybe Alignment -> String
containerClass classPrefix alignment =
    case alignment of
        Just Above ->
            classPrefix ++ "options-container-above"

        Just Below ->
            classPrefix ++ "options-container-below"

        Nothing ->
            classPrefix ++ "invisible"


selectClass : String -> Alignment -> String
selectClass classPrefix alignment =
    case alignment of
        Above ->
            classPrefix ++ "opened-above"

        Below ->
            classPrefix ++ "opened-below"


getElements : String -> String -> Task Dom.Error Params
getElements containerId selectId =
    Task.succeed (\container select viewport -> Params { container = container, select = select, viewport = viewport })
        |> TaskExtra.andMap (Dom.getElement containerId)
        |> TaskExtra.andMap (Dom.getElement selectId)
        |> TaskExtra.andMap Dom.getViewport



-- Test


isAbove : Alignment -> Bool
isAbove alignment =
    case alignment of
        Above ->
            True

        Below ->
            False


params : { container : Element, select : Element, viewport : Dom.Viewport } -> Params
params =
    Params
