# elm-smart-select
A select component written in Elm 0.19

## Install
elm install mercurymedia/elm-smart-select

## In Action

#### Single Select

![SingleSelect](https://user-images.githubusercontent.com/20546636/66810094-dace5480-ef2e-11e9-9e9a-df1fddb38dc6.gif)

#### Multi Select

![MultiSelect](https://user-images.githubusercontent.com/20546636/66810122-e1f56280-ef2e-11e9-9ac6-f2de80802a58.gif)

## Usage
This package exposes four modules `SingleSelect`, `SingleSelectRemote`, `MultiSelect` and `MultiSelectRemote`. The `Single` pickers can be used to pick a single element while the `Multi` pickers are used to select a list of elements. The pickers without a suffix select from preloaded data whereas the `Remote` pickers query a remote source. To keep things simple, the documentation here focuses on the `SingleSelect`. **_Note:_** While the basic architecture across all of the modules is similar, certain functions may expect different arguments from one module to the next. Please refer to the specific module documentation for further details and information.

There are 6 steps to configure a `SmartSelect`:

1. Import the select and add it to your model providing your local `Msg` type and the datatype of the data to be selected.

```elm
import SingleSelect

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
            [ SingleSelect.view { selected = model.selectedProduct, options = model.products, optionLabelFn = .name } model.select ]
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

`SingleSelect.upate` returns an updated smart select instance and a cmd.

6. Setup the select subscription. The select module uses a subscription to determine when to close (outside of a selection). Wire the picker subscription like below.

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    SingleSelect.subscriptions model.select
```

## Examples

Examples can be found in the [examples](https://github.com/mercurymedia/elm-smart-select/tree/master/examples) folder. To build the examples to view in the browser run: `cd examples && make && cd ..` from the root of the repository.

## CSS

The CSS for smart select is distributed separately and can be found [here](https://github.com/mercurymedia/elm-smart-select/tree/master/css)
