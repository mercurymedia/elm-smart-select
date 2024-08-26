module SmartSelect.Settings exposing
    ( Settings
    , Theme
    , defaultSettings
    , defaultTheme
    )

import Css


type alias Settings =
    { theme : Theme }


defaultSettings : Settings
defaultSettings =
    { theme = defaultTheme }


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
    }


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
    }
