module SmartSelect.Utilities exposing
    ( KeyCode(..)
    , RemoteQueryAttrs
    , decodeOptions
    , eventIsOutsideComponent
    , newFocusedOptionIndexAfterSelection
    , preventDefault
    , spinnerConfig
    , toKeyCode
    )

{-| Utilities shared by the SmartSelect modules.


# SmartSelect settings

@docs ApiSearchAttrs

-}

import Color
import Http exposing (Header)
import Json.Decode as Decode exposing (Decoder)
import Spinner


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
    | Escape
    | Other


preventDefault : KeyCode -> Bool
preventDefault key =
    key == Up || key == Down


toKeyCode : String -> KeyCode
toKeyCode string =
    case string of
        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        "Enter" ->
            Enter

        "Escape" ->
            Escape

        _ ->
            Other


decodeOptions : Decoder a -> Decoder (List a)
decodeOptions decoder =
    Decode.list decoder
