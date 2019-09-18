module SmartSelect.Utilities exposing
    ( SearchUnion(..)
    , KeyCode(..), decodeOptions, eventIsOutsideComponent, newFocusedIndexAfterSelection, preventDefault, spinnerConfig, toKeyCode
    )

{-| Utilities shared by the SmartSelect modules.


# SmartSelect settings

@docs SearchUnion, ApiSearchAttrs

-}

import Color
import Http exposing (Header)
import Json.Decode as Decode exposing (Decoder)
import Spinner


{-| The `SmartSelect` can execute searches on either a `Local` or `API` basis. If the data you wish to select from has already been loaded onto the model or only exists on the frontend, use `Local`.
It expects a function that takes in a search string and returns a list of data to be rendered. If you wish to query/search data from an external source for your selection set, use `API`.
`API` expects a tuple with a `Color.Color` to indicate what color the loading spinner should be, as well as attributes to help execute the proper query.
-}
type SearchUnion a
    = Local (String -> List a)
    | API ( Color.Color, ApiSearchAttrs a )


{-| Fields to be provided to the `API` constructor of the search union to facilitate the external request. The function provided to url takes in searchText in the event it is necessary for the query.
-}
type alias ApiSearchAttrs a =
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


newFocusedIndexAfterSelection : Int -> Int
newFocusedIndexAfterSelection currentFocusedIdx =
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
