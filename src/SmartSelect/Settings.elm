module SmartSelect.Settings exposing (Settings, BackgroundScrollBehavior(..), RemoteSettings, RemoteQueryAttrs, Theme, defaultRemoteSettings, defaultSettings, defaultTheme)

{-| The settings module for all smart select variants


# Architecture

@docs Settings, BackgroundScrollBehavior, RemoteSettings, RemoteQueryAttrs, Theme, defaultRemoteSettings, defaultSettings, defaultTheme

-}

import Color
import Css
import Html exposing (Html)
import Http exposing (Header)
import Json.Decode exposing (Decoder)


{-| The type representing the select's general settings to be passed to all view functions
-}
type alias Settings msg =
    { theme : Theme
    , isDisabled : Bool
    , optionsContainerMaxHeight : Float
    , placeholder : String
    , noResultsForMsg : String -> String
    , noOptionsMsg : String
    , icon : Maybe (Html msg)
    , id : String
    , bgScrollBehavior : BackgroundScrollBehavior
    }


{-| The type representing the select's popover behaviour when scrolling in the background.
-}
type BackgroundScrollBehavior
    = BlockScrolling
    | CloseOnScroll
    | KeepOpen


{-| The type representing the select's remote settings (including the general settings) to be passed to all view & update functions of the remote variants
-}
type alias RemoteSettings msg =
    { settings : Settings msg
    , spinnerColor : Color.Color
    , characterThresholdPrompt : Int -> String
    , characterSearchThreshold : Int
    , debounceDuration : Float
    , queryErrorMsg : String
    }


{-| Fields to be provided to facilitate the external request. The function provided to url takes in searchText in the event it is necessary for the query.
-}
type alias RemoteQueryAttrs a =
    { headers : List Header
    , url : String -> String
    , optionDecoder : Decoder a
    }


{-| The type facilitating the Theme with the most important design tokens
-}
type alias Theme =
    { fontSize :
        { base : Css.Px
        , sm : Css.Px
        , xs : Css.Px
        , xxs : Css.Px
        }
    , color :
        { text :
            { primary : Css.Color
            , secondary : Css.Color
            , disabled : Css.Color
            }
        , primary :
            { main : Css.Color
            , contrastText : Css.Color
            , light : Css.Color
            }
        , error :
            { main : Css.Color
            , contrastText : Css.Color
            , light : Css.Color
            }
        , background :
            { input : Css.Color
            , optionsContainer : Css.Color
            }
        , action : { hover : Css.Color }
        , border : Css.Color
        }
    , size :
        { iconButton : Css.Px
        , inputElement : Css.Px
        }
    , borderWidth : Css.Px
    , borderRadius :
        { base : Css.Px
        , lg : Css.Px
        }
    , boxShadow :
        { offsetX : Css.Px
        , offsetY : Css.Px
        , blurRadius : Css.Px
        , spreadRadius : Css.Px
        , color : Css.Color
        }
    , transition : { duration : Float }
    , zIndex : Int
    , className : String
    , classNamePrefix : String
    }


{-| A record of default settings for all the select variants. Extend this if
you want to further customize.
-}
defaultSettings : Settings msg
defaultSettings =
    { theme = defaultTheme
    , isDisabled = False
    , optionsContainerMaxHeight = 300
    , placeholder = "Search..."
    , noResultsForMsg = \_ -> "No results"
    , noOptionsMsg = "No options available"
    , icon = Nothing
    , id = "elm-smart-select"
    , bgScrollBehavior = BlockScrolling
    }


{-| A record of default settings for the remote select variants. Extend this if
you want to further customize.
-}
defaultRemoteSettings : RemoteSettings msg
defaultRemoteSettings =
    { settings = defaultSettings
    , spinnerColor = Color.rgb255 57 179 181
    , characterThresholdPrompt = \_ -> "Enter at least 2 characters to search..."
    , characterSearchThreshold = 2
    , debounceDuration = 1000
    , queryErrorMsg = "Error"
    }


{-| The default theme that is included in the defaultSettings
-}
defaultTheme : Theme
defaultTheme =
    { fontSize =
        { base = Css.px 16
        , sm = Css.px 14
        , xs = Css.px 12
        , xxs = Css.px 10
        }
    , color =
        { text =
            { primary = Css.hex "22292f"
            , secondary = Css.rgba 0 0 0 0.5
            , disabled = Css.rgba 0 0 0 0.25
            }
        , primary =
            { main = Css.hex "3490dc"
            , contrastText = Css.hex "ffffff"
            , light = Css.rgba 52 144 220 0.1
            }
        , error =
            { main = Css.hex "c53030"
            , contrastText = Css.hex "ffffff"
            , light = Css.hex "feb2b2"
            }
        , background =
            { input = Css.hex "ffffff"
            , optionsContainer = Css.hex "ffffff"
            }
        , action = { hover = Css.rgba 0 0 0 0.03 }
        , border = Css.rgba 0 0 0 0.1
        }
    , size =
        { iconButton = Css.px 24
        , inputElement = Css.px 18
        }
    , borderWidth = Css.px 1
    , borderRadius =
        { base = Css.px 3
        , lg = Css.px 6
        }
    , boxShadow =
        { offsetX = Css.px 0
        , offsetY = Css.px 0
        , blurRadius = Css.px 5
        , spreadRadius = Css.px 0
        , color = Css.rgba 0 0 0 0.25
        }
    , transition = { duration = 300 }
    , zIndex = 1000
    , className = ""
    , classNamePrefix = "elm-smart-select"
    }
