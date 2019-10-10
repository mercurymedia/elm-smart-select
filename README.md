# elm-smart-select
A select component written in Elm 0.19

## Install
elm install mercurymedia/elm-smart-select

## Usage
This package exposes four modules `SingleSelectLocal`, `SingleSelectRemote`, `MultiSelectLocal` and `MultiSelectRemote`. The `Single` pickers can be used to pick a single element while the `Multi` pickers are used to select a list of elements. The `Local` pickers select from preloaded data where as the `Remote` pickers query a remote source. To keep things simple, the documentation here focuses on the `SingleSelectLocal`. For more information, refer to the module documentation and examples.

There are 7 steps to configure a `SmartSelect`:

1. Import the select and add it to your model providing the datatype of the data to be selected.

```elm
import SingleSelectLocal as Select

type alias Product =
    { id : Int
    , name : String
    , price : String
    }

type alias Model =
    { products : List Product
    , select : Select.SmartSelect Msg Product
    }
```

2. Define a `Msg` to handle updates from the select.

```elm
type Msg
    = ...
    | HandleSelectUpdate (Select.Msg Product)
```

3. Configure the `Settings` for the select. The `internalMsg` field takes a function that expects a `Select.Msg` and returns the `Msg` we defined in step 2. Please refer to the module documentation for more information on the settings.

```elm
selectSettings : Select.Settings Msg Product
selectSettings =
    { internalMsg = \msg -> HandleSelectUpdate msg
    , searchFn =
        \searchText products ->
            List.filter (\product -> String.contains (String.toLower searchText) (String.toLower product.name)) products
    , optionType = "Product"
    , optionLabel = \product -> product.name
    , optionDescription = \product -> product.price
    , optionsContainerMaxHeight = 300
    , optionHeight = 50
    , closeOnSelect = True
    }
```

4. Initialize the select. The `init` function takes in the select settings and a previously selected element, if any. In the case of the `Single` pickers, the selected element is a `Maybe a` whereas in the `Multi` pickers selected is a `List a`.

```elm
init : ( Model, Cmd Msg )
init =
    ( { products = products
      , select = Select.init selectSettings Nothing
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

5. View the select. Call `Select.view`. **_Note:_** The `view` functions for each of the modules expect slightly different arguments from one another. Please refer to the relevant module documentation for further information.

```elm
view : Model -> Html Msg
view model =
    div []
        [ ...
        , div
            [ style "width" "500px" ]
            [ Select.view False model.products model.select ]
        ]
```

6. Update the select. Here is where we handle the `Msg` we defined in step 2. As indicated before, the message carries with it a `Select.Msg` for updating the select component. 

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ...

        HandleSelectUpdate sMsg ->
            let
                ( updatedSelect, selectCmd ) =
                    SmartSelect.update sMsg model.singleSelect
            in
            ( { model | singleSelect = updatedSelect }, selectCmd )
```

`SingleSelect.upate` returns an updated smart select instance and a cmd.

You might be wondering were the selected state is. The smart select stores the select state and exposes a `selected` method to retrieve it. Call this method when you need the selected entity/entities.

7. Setup the select subscription. The select module uses a subscription to determine when to close (outside of a selection). Wire the picker subscription like below.

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Select.subscriptions model.select
```

## Examples

Examples can be found in the [examples](https://github.com/mercurymedia/elm-smart-select/tree/master/examples) folder. To build the examples to view in the browser run: `cd examples && make && cd ..` from the root of the repository.

## CSS

The CSS for the date picker is distributed separately and can be found [here](https://github.com/mercurymedia/elm-smart-select/tree/master/css)