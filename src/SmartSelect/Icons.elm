module SmartSelect.Icons exposing (Icon(..), chevronDown, toHtml, withSize, withStrokeWidth, x)

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)



-- The code below comes from feathericons/elm-feather (https://github.com/feathericons/elm-feather/)


{-| Customizable attributes of icon
-}
type alias IconAttributes =
    { size : Float
    , sizeUnit : String
    , strokeWidth : Float
    , class : Maybe String
    , viewBox : String
    }


{-| Default attributes, first argument is icon name
-}
defaultAttributes : String -> IconAttributes
defaultAttributes name =
    { size = 24
    , sizeUnit = ""
    , strokeWidth = 2
    , class = Just <| "feather feather-" ++ name
    , viewBox = "0 0 24 24"
    }


{-| Opaque type representing icon builder
-}
type Icon
    = Icon
        { attrs : IconAttributes
        , src : List (Svg Never)
        }


{-| Set size attribute of an icon
Icon.download
|> Icon.withSize 10
|> Icon.toHtml []
-}
withSize : Float -> Icon -> Icon
withSize size (Icon { attrs, src }) =
    Icon { attrs = { attrs | size = size }, src = src }


{-| Set thickness of icon lines, useful when inlining icons with bold / normal text.
Icon.playCircle
|> Icon.withStrokeWidth 1
|> Icon.toHtml []
-}
withStrokeWidth : Float -> Icon -> Icon
withStrokeWidth strokeWidth (Icon { attrs, src }) =
    Icon { attrs = { attrs | strokeWidth = strokeWidth }, src = src }


{-| Build icon, ready to use in html. It accepts list of svg attributes, for example in case if you want to add an event handler.
-- default
Icon.download
|> Icon.toHtml []
-- with some attributes
Icon.download
|> Icon.withSize 10
|> Icon.withClass "icon-download"
|> Icon.toHtml [ onClick Download ]
-}
toHtml : List (Svg.Attribute msg) -> Icon -> Html msg
toHtml attributes (Icon { src, attrs }) =
    let
        strSize =
            attrs.size |> String.fromFloat

        baseAttributes =
            [ fill "none"
            , height <| strSize ++ attrs.sizeUnit
            , width <| strSize ++ attrs.sizeUnit
            , stroke "currentColor"
            , strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth <| String.fromFloat attrs.strokeWidth
            , viewBox attrs.viewBox
            ]

        combinedAttributes =
            (case attrs.class of
                Just c ->
                    class c :: baseAttributes

                Nothing ->
                    baseAttributes
            )
                ++ attributes
    in
    src
        |> List.map (Svg.map never)
        |> svg combinedAttributes


makeBuilder : String -> List (Svg Never) -> Icon
makeBuilder name src =
    Icon { attrs = defaultAttributes name, src = src }


{-| x
![image](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDI0IDI0IiBmaWxsPSJub25lIiBzdHJva2U9ImN1cnJlbnRDb2xvciIgc3Ryb2tlLXdpZHRoPSIyIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxsaW5lIHgxPSIxOCIgeTE9IjYiIHgyPSI2IiB5Mj0iMTgiPjwvbGluZT48bGluZSB4MT0iNiIgeTE9IjYiIHgyPSIxOCIgeTI9IjE4Ij48L2xpbmU+PC9zdmc+)
-}
x : Icon
x =
    makeBuilder "x"
        [ Svg.line [ x1 "18", y1 "6", x2 "6", y2 "18" ] []
        , Svg.line [ x1 "6", y1 "6", x2 "18", y2 "18" ] []
        ]


{-| chevron-down

![image](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDI0IDI0IiBmaWxsPSJub25lIiBzdHJva2U9ImN1cnJlbnRDb2xvciIgc3Ryb2tlLXdpZHRoPSIyIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwb2x5bGluZSBwb2ludHM9IjYgOSAxMiAxNSAxOCA5Ij48L3BvbHlsaW5lPjwvc3ZnPg==)

-}
chevronDown : Icon
chevronDown =
    makeBuilder "chevron-down"
        [ Svg.polyline [ points "6 9 12 15 18 9" ] []
        ]
