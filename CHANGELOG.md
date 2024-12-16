# Changelog

## [7.0.0]

**MAJOR CHANGE**
- A new property has been added to the `Settings` record:
  
  `bgScrollBehaviour` - defines the behaviour of the Popover element when the user scrolls in the background. The options are based on a new type `BackgroundScrollBehavior` consisting of `BlockScrolling`, `CloseOnScroll` and `KeepOpen`

- Check the Readme for more informations


## [6.0.0]

**MAJOR CHANGE**
- New properties have been added to the `Theme` record
  1. `className` - a String that will be attached to the outer container element of the smart select
  2. `zIndex`- an Int that defines the `z-index` on the options-container element
- The `width` of the container elment has been fixed to `100%`
- A new setting `icon` has been added to the `Settings` record - you can pass your custom icon or HTML to it. Default is a chevron-down.

## [5.0.0]

**MAJOR CHANGE**
- All view functions have been moved to a shared module to reduce duplicate code between the `SingleSelect`, `SingleSelectRemote`, `MultiSelect`, `MultiSelectRemote`.
- The CSS is now defined in a built-in way with `elm-css`, all separately distributed styles have been removed. The CSS classes are still available and attached to all of the components. So in case more individual styling is needed, you can still use the classes – even though the markup and classNames might have changed.
- The select's shared settings have been unified into a module `Settings` for all the select variants. There is a general `type alias Settings` and an extended `type alias RemoteSettings` to be used with the according select variants. The `Settings` module itself is now an `exposed-module` that needs to be imported separately and passed to the select's view functions like this:

    ```elm
    import SingleSelect
    import SmartSelect.Settings exposing (defaultSettings)

    [...]

    view : Model -> Html Msg
    view model =
        SingleSelect.view
            { selected = model.selectedProduct
            , options = model.products
            , optionLabelFn = .name
            , settings = defaultSettings
            }
            model.select

    ``` 
  
- To allow custom styling, there's a newly created `Theme` as part of the `Settings`. A predefined `defaultTheme` is included in the `defaultSettings` but as all other settings it can be overwritten (see README.md). 
- Added examples for `SingleSelectRemote` and `MultiSelectRemote` to demonstrate usage with remote data (example API from [freetestapi.com](https://freetestapi.com))

**CHANGED**
- Improved UX / restructured elements: the text input field is no longer rendered as part of the popover element in the SingleSelect variants. The search text input and selected value display is accessible in one form field element. The popover only displays options.
- Improved styling: some clean up of CSS-classes and visual design adjustments. Also extracted some CSS-variables as some kind of theme to make it easily customizable (see `/css/SmartSelect.css`).
- Improved Positioning: all variants' popover elements are now rendered outside the DOM hierarchy and positioned automatically according to available viewport space in each direction. Hopefully no more `z-index` problems.
- Refactoring: now sharing more functions between all variants. Also extracted almost all markup in reusable view-functions (see `/src/SmartSelect/ViewComponents.elm`)

## [4.0.0]

... (changelog not maintained until here)