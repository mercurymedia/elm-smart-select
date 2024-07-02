# Changelog

## [5.0.0]

**ADDED**
- Added examples for `SingleSelectRemote` and `MultiSelectRemote` to demonstrate usage with remote data (example API from [freetestapi.com](https://freetestapi.com))

**CHANGED**
- Improved UX / restructured elements: the text input field is no longer rendered as part of the popover element in the SingleSelect variants. The search text input and selected value display is accessible in one form field element. The popover only displays options.
- Improved styling: some clean up of CSS-classes and visual design adjustments. Also extracted some CSS-variables as some kind of theme to make it easily customizable (see `/css/SmartSelect.css`).
- Improved Positioning: all variants' popover elements are now rendered outside the DOM hierarchy and positioned automatically according to available viewport space in each direction. Hopefully no more `z-index` problems.
- Refactoring: now sharing more functions between all variants. Also extracted almost all markup in reusable view-functions (see `/src/SmartSelect/ViewComponents.elm`)

## [4.0.0]

... (changelog not maintained until here)