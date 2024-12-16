# elm-smart-select
A select component written in Elm 0.19

## Install
elm install mercurymedia/elm-smart-select

## In Action

#### Single Select

![singleselect](https://github.com/markus-mind/elm-smart-select/assets/32676430/92e3ed36-39c0-4e5d-bf21-b1e5b0f566ce)

#### Multi Select

![multiselect](https://github.com/markus-mind/elm-smart-select/assets/32676430/1ab8823a-a79b-4ce4-8cb7-f951a96ab070)

## Usage
This package exposes four modules `SingleSelect`, `SingleSelectRemote`, `MultiSelect`, `MultiSelectRemote` and `SmartSelect.Settings`. The `Single` pickers can be used to pick a single element while the `Multi` pickers are used to select a list of elements. The pickers without a suffix select from preloaded data whereas the `Remote` pickers query a remote source. To keep things simple, the documentation here focuses on the `SingleSelect`. **_Note:_** While the basic architecture across all of the modules is similar, certain functions may expect different arguments from one module to the next. Please refer to the specific module documentation for further details and information.

There are 6 steps to configure a `SmartSelect`:

1. Import the select and its `Settings` and add it to your model providing your local `Msg` type and the datatype of the data to be selected.

```elm
import SingleSelect
import SmartSelect.Settings exposing (defaultSettings)

type alias Product =
    { id : Int
    , name : String
    , price : String
    }

type alias Model =
    { products : List Product
    , select : SingleSelect.SmartSelect Msg Product
    , selectedProduct : Maybe Product
    }
```

2. Define two `Msg`s: one to handle updates internal to the select and one to handle receiving a selection from the select.

```elm
type Msg
    = HandleSelectUpdate (SingleSelect.Msg Product)
    | HandleSelection ( Product, SingleSelect.Msg )
```

3. Initialize the select. As noted above, please refer to documentation for the specific arguments that the `init` function of a particular module takes.

```elm
init : ( Model, Cmd Msg )
init =
    ( { products = products
      , select = SingleSelect.init
            { selectionMsg = HandleSelection
            , internalMsg = HandleSelectUpdate
            , idPrefix = "my-prefix"
            }
      , selectedProduct = Nothing
      }
    )

products : List Product
products =
    [ { id = 1
      , name = "product 1"
      , price = "$3.00"
      }
    , { id = 2
      , name = "product 2"
      , price = "$5.00"
      }
    ...
    ]

```

4. View the select. Call `SingleSelect.view`.

Each module exposes a `.view` and `.viewCustom` function. `.view` takes only the arguments it needs while providing reasonable defaults for other view related settings. `.viewCustom` expects all of the fields that can be customized to be provided as arguments. Please refer to the module documentation for more details.

```elm
view : Model -> Html Msg
view model =
    div []
        [ ...
        , div
            [ style "width" "500px" ]
            [ SingleSelect.view 
                { selected = model.selectedProduct
                , options = model.products
                , optionLabelFn = .name
                , settings = defaultSettings 
                } 
                model.select
            ]
        ]
```

5. Update the select. Here is where we handle the `Msg`s we defined in step 2. 

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ...

        HandleSelectUpdate sMsg ->
            let
                ( updatedSelect, selectCmd ) =
                    SingleSelect.update sMsg model.select
            in
            ( { model | select = updatedSelect }, selectCmd )

        HandleSelection ( product, sMsg ) ->
            let
                ( updatedSelect, selectCmd ) =
                    SingleSelect.update sMsg model.select
            in
            ( { model | selectedProduct = product, select = updatedSelect }, selectCmd )
```

`SingleSelect.update` returns an updated smart select instance and a cmd.

6. Setup the select subscription. The select module uses a subscription to determine when to close (outside of a selection). Wire the picker subscription like below.

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    SingleSelect.subscriptions model.select
```

## Examples

Examples can be found in the [examples](https://github.com/mercurymedia/elm-smart-select/tree/master/examples) folder. To view the examples run `npm install` and `npm start`. Open your browser at [localhost:1234](http://localhost:1234).

## Popover Positioning and Background Scroll Behaviour

The Select's popover element is manually positioned, meaning the element has the style `position: fixed;` to avoid annoying overflow- or z-index-issues when deeply nested in the DOM. It's coordinates are calculated based on the trigger element's (the select's input field) position once the select is being clicked and the popover opens. The calculation takes the available screen size in each direction into account so that the popover element always opens in the direction with enough space.

Unfortunately the manual positioning comes with a disadvantage that can't be resolved with CSS-only: if the select is being rendered as part of a scroll container, its popover won't automatically move and follow its trigger element. It will stick to the position that has been calculated at the time of its opening.
This package offers a few solutions to avoid that behavior:

- **The simple way:** the `Settings` record has a property `bgScrollBehavior` which can be used to define what happens with the popover when the user scrolls in the select's background - no matter if the select element is part of a scroll container or not. When the popover opens there is also an invisible backdrop element opening that can be configured to catch and deal with background scroll events. There are 3 options from the type `BackgroundScrollBehavior`:
  
  - `BlockScrolling`: when opened, the backdrop element prevents the user from scrolling in the background so all elements stay at the same position.
  - `CloseOnScroll`: as soon as the user starts scrolling in the background, the popover closes and the scroll-container is being scrolled as usual.
  - `KeepOpen`: see above - the popover element just sticks to its original position while its trigger element might scroll away.
- **The manual approach:** when using `KeepOpen` there is one more option you can implement, even though it is a little more work to do: all the select variants have a `updatePosition` function. So you can listen to scroll events on your app's scroll container and once they're fired you can call the function to re-trigger the popover's position calculation. The popover should then follow its trigger element.



## CSS & Theming

The CSS for the date picker is now defined in a built-in way using [elm-css](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/).
There are some design tokens that can be configured individually in a theme.
In case you need to add additional styling, you can use the CSS-classes that are attached to all the components.

In case you'd like to use the Theme, you can pass your custom theme to the `Settings`. The `Theme` record currently looks like this:

```elm
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
```

Passing a customized theme to the settings works like this:

```elm
import Css -- from elm-css
import DatePicker.Settings
    exposing
        ( Settings
        , Theme
        , defaultSettings
        , defaultTheme
        )

-- [...]

customTheme : Theme
customTheme =
    { defaultTheme
        | color =
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
    }


customRemoteSettings : Settings
customRemoteSettings = 
    let
        defaults =
            defaultSettings zone
    in
    { defaults
        | -- [...]
        , theme = customTheme
    }

```
