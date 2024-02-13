module SmartSelect.Alignment exposing (Alignment(..), containerClass, init, selectClass)

import Browser.Dom exposing (Element)


type Alignment
    = Above
    | Below


init : { container : Element, select : Element } -> Alignment
init { container, select } =
    if
        select.viewport.y
            + select.element.y
            + select.element.height
            + container.element.height
            >= select.viewport.height
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
