# elm-smart-select
A select component written in Elm 0.19

## Install
elm install mercurymedia/elm-datetime-picker

## Usage
This package exposes two modules `SmartSelectSingle` and `SmartSelectMulti`. As their names imply, `SmartSelectSingle` can be used to pick a single element while `SmartSelectMulti` is used to select a list of elements. To keep things simple, the documentation here focuses on the `SmartSelectSingle` but both are implemented in the example app for additional reference.

There are 7 steps to configure the `SmartSelect`:

1. Import the SmartSelect and SmartSelect.Utilities module and expose the `SearchUnion` type. We will get back to why we need the `SearchUnion` later.

```elm
import SmartSelectSingle as SingleSelect
import SmartSelect.Utilities exposing (SearchUnion(..))
```

2. Add the smart select to the model providing the datatype of the data to be selected and initialize it in the model init. The `init` function takes in the select settings (see step 4) and a previously selected element, if any. **_Note:_** The selection state for the `SmartSelectMulti` is a List of selected entities as opposed to a Maybe entity for `SmartSelectSingle`.

```elm
type alias Product =
    { id : Int
    , name : String
    , price : String
    }

type alias Model =
    { ...
    , singleSelect : SingleSelect.SmartSelect Msg Product
    }

init : ( Model, Cmd Msg )
init =
    ( { ...
      , singleSelect = SingleSelect.init {selectSettings} {previouslySelected}
      }
    )
```

3. One message needs to be defined indicating that an update to the smart select has been triggered and needs to be handled. This message expects a `SingleSelect.Msg`.

```elm
type Msg
    = ...
    | HandleSelectUpdate (SingleSelect.Msg Product)
```

4. Configure the settings for the smart select. The `Settings` type as defined in the `SmartSelectSingle` module is below:

```elm
type alias Settings msg a =
    { internalMsg : Msg a -> msg
    , optionType : String
    , optionLabel : a -> String
    , optionDescription : a -> String
    , searchFn : SearchUnion a
    , debounceDuration : Float
    , characterSearchThreshold : Int
    , closeOnSelect : Bool
    }
```

- The `internalMsg` field takes a function that expects a SmartSelect.Msg and returns the msg we defined in step 2.
- `optionType` is a string that indicates what kind of data is being selected, i.e. "Product" or "Client"
- `optionLabel` expects an instance of the data being selected from and returns a string naming/labeling the instance, i.e. if it is a "Product" being selected, the label may be "Garden Hose"
- `optionDescription` expects an instance of the data being selected from and returns a string describing the instance, i.e. if the label is "Garden Hose", the description may be "30 ft"
    - Because the smart select is unaware of the type and structure of the data it is processing, these functions are necessary to help render the options in the select dropdown.
- `debounceDuration` indicates how long if at all to wait between the last keypress and executing a search. This is particularly useful if the search being executed is pinging an external source.
- `characterThreshold` indicates how many if any characters should be typed before a search is executed.
- `closeOnSelect` indicates whether or not the `SmartSelect` should close itself after a selection has been made.

The `searchFn` field expects a `SearchUnion`. The `SearchUnion` type as defined in the `SmartSelect.Utilities` module is below:

```elm
type SearchUnion a
    = Local (String -> List a)
    | API (Color.Color, ApiSearchAttrs a)
```

The `SmartSelect` can execute searches on either a `Local` or `API` basis. If the data you wish to select from has already been loaded onto the model or only exists on the frontend, use `Local`.
It expects a function that takes in a search string and returns a list of data to be rendered. If you wish to query/search data from an external source for your selection set, use `API`. `API` expects a tuple with a `Color.Color` to indicate what color the loading spinner should be, as well as attributes to help execute the proper query. The `ApiSearchAttrs` type as defined in the `SmartSelect.Utilities` module can be seen below:

```elm
type alias ApiSearchAttrs a =
    { headers : List Http.Header
    , url : String -> String
    , optionDecoder : Json.Decode.Decoder a
    }
```

5. Viewing the select. Call `SingleSelect.view`. **_Note:_** The `view` for the multi select expects different arguments than seen here. Please refer to the `SmartSelectMulti` documentation for further information.

`SingleSelect.view` expects the following (in order):
- a boolean indicating if the select is disabled or not
- the smart select instance

6. Updating the select. Here is where we handle the `Msg` we defined in step 3. As indicated before, the message carries with it a `SingleSelect.Msg` for updating the select component. 

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

7. The select module uses a subscription to determine when to close (outside of a selection). Wire the picker subscription like below.

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    SingleSelect.subscriptions model.singleSelect
```

## Examples

Examples can be found in the [examples](https://github.com/mercurymedia/elm-smart-select/tree/master/examples) folder. To build the examples to view in the browser run: `cd examples && make && cd ..` from the root of the repository.

## CSS

The CSS for the date picker is distributed separately and can be found [here](https://github.com/mercurymedia/elm-smart-select/tree/master/css)