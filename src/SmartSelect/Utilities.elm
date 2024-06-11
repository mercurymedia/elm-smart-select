module SmartSelect.Utilities exposing
    ( KeyCode(..)
    , RemoteQueryAttrs
    , alwaysStopPropogation
    , blurInput
    , clickedOutsideSelect
    , decodeOptions
    , eventIsOutsideComponent
    , focusInput
    , newFocusedOptionIndexAfterSelection
    , preventDefault
    , scrollToOption
    , search
    , spinnerConfig
    , toKeyCode
    )

{-| Utilities shared by the SmartSelect modules.


# SmartSelect settings

@docs ApiSearchAttrs

-}

import Browser.Dom as Dom
import Color
import Http exposing (Header)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData)
import SmartSelect.Id as Id
import Spinner
import Task


{-| Fields to be provided to facilitate the external request. The function provided to url takes in searchText in the event it is necessary for the query.
-}
type alias RemoteQueryAttrs a =
    { headers : List Header
    , url : String -> String
    , optionDecoder : Decoder a
    }


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


clickedOutsideSelect : String -> msg -> Decode.Decoder msg
clickedOutsideSelect componentId msg =
    Decode.field "target" (eventIsOutsideComponent componentId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| msg

                else
                    Decode.fail "inside component"
            )


eventIsOutsideComponent : String -> Decode.Decoder Bool
eventIsOutsideComponent componentId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if componentId == id then
                        -- found match by id
                        Decode.succeed False

                    else
                        -- try next decoder
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> eventIsOutsideComponent componentId |> Decode.field "parentNode")

        -- fallback if all previous decoders failed
        , Decode.succeed True
        ]


focusInput : Id.Prefix -> msg -> Cmd msg
focusInput prefix onErrorMsg =
    Task.attempt (\_ -> onErrorMsg) (Dom.focus (Id.input prefix))


blurInput : Id.Prefix -> msg -> Cmd msg
blurInput prefix onErrorMsg =
    Task.attempt (\_ -> onErrorMsg) (Dom.blur (Id.input prefix))


scrollToOption : msg -> Id.Prefix -> Int -> Cmd msg
scrollToOption onErrorMsg prefix idx =
    Task.attempt (\_ -> onErrorMsg) (scrollTask prefix idx)


scrollTask : Id.Prefix -> Int -> Task.Task Dom.Error ()
scrollTask prefix idx =
    Task.sequence
        [ Dom.getElement (Id.option prefix idx) |> Task.map (\x -> x.element.y)
        , Dom.getElement (Id.option prefix idx) |> Task.map (\x -> x.element.height)
        , Dom.getElement (Id.container prefix) |> Task.map (\x -> x.element.y)
        , Dom.getElement (Id.container prefix) |> Task.map (\x -> x.element.height)
        , Dom.getViewportOf (Id.container prefix) |> Task.map (\x -> x.viewport.y)
        ]
        |> Task.andThen
            (\outcome ->
                case outcome of
                    optionY :: optionHeight :: containerY :: containerHeight :: containerScrollTop :: [] ->
                        if (optionY + optionHeight) >= containerY + containerHeight then
                            Dom.setViewportOf (Id.container prefix) 0 (containerScrollTop + ((optionY - (containerY + containerHeight)) + optionHeight))
                                |> Task.onError (\_ -> Task.succeed ())

                        else if optionY < containerY then
                            Dom.setViewportOf (Id.container prefix) 0 (containerScrollTop + (optionY - containerY))
                                |> Task.onError (\_ -> Task.succeed ())

                        else
                            Task.succeed ()

                    _ ->
                        Task.succeed ()
            )


newFocusedOptionIndexAfterSelection : Int -> Int
newFocusedOptionIndexAfterSelection currentFocusedIdx =
    if currentFocusedIdx > 0 then
        currentFocusedIdx - 1

    else
        0


type KeyCode
    = Up
    | Down
    | Enter
    | Backspace
    | Escape
    | Other


preventDefault : KeyCode -> Bool
preventDefault key =
    key == Up || key == Down || key == Enter


alwaysStopPropogation : msg -> ( msg, Bool )
alwaysStopPropogation msg =
    ( msg, True )


toKeyCode : String -> KeyCode
toKeyCode string =
    case string of
        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        "Enter" ->
            Enter

        "Backspace" ->
            Backspace

        "Escape" ->
            Escape

        _ ->
            Other


search : { remoteQueryAttrs : RemoteQueryAttrs a, handleResponse : RemoteData Http.Error (List a) -> msg } -> String -> Cmd msg
search { remoteQueryAttrs, handleResponse } searchText =
    Http.request
        { method = "GET"
        , headers = remoteQueryAttrs.headers
        , url = remoteQueryAttrs.url searchText
        , body = Http.emptyBody
        , expect = Http.expectJson (\results -> RemoteData.fromResult results |> handleResponse) (decodeOptions remoteQueryAttrs.optionDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


decodeOptions : Decoder a -> Decoder (List a)
decodeOptions decoder =
    Decode.list decoder
