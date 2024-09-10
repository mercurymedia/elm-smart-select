module AlignmentTest exposing (suite)

import Browser.Dom as Dom exposing (Element)
import Expect
import Html.Attributes as Attrs
import SmartSelect.Alignment as Alignment
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Alignment"
        [ test "Should return Above when the container element does not fit below the select element" <|
            \_ ->
                let
                    scene =
                        { height = 100, width = 100 }

                    viewport =
                        { height = 100, width = 100, x = 0, y = 0 }

                    select : Element
                    select =
                        { element = { height = 10, width = 10, x = 30, y = 80 }
                        , scene = scene
                        , viewport = viewport
                        }

                    container : Element
                    container =
                        { element = { height = 30, width = 10, x = 0, y = 0 }
                        , scene = scene
                        , viewport = viewport
                        }

                    domViewport : Dom.Viewport
                    domViewport =
                        { scene = scene
                        , viewport = viewport
                        }

                    alignment =
                        Alignment.init { container = container, select = select, viewport = domViewport }

                    expected =
                        [ Attrs.style "position" "fixed"
                        , Attrs.style "bottom" "20px"
                        , Attrs.style "left" "30px"
                        , Attrs.style "width" "10px"
                        ]
                in
                Expect.equal (Alignment.style (Just alignment)) expected
        , test "Should return Below when the container element fit below the select element" <|
            \_ ->
                let
                    select : Element
                    select =
                        { element = { height = 10, width = 10, x = 30, y = 30 }
                        , scene = { height = 100, width = 100 }
                        , viewport = { height = 100, width = 100, x = 0, y = 0 }
                        }

                    container : Element
                    container =
                        { element = { height = 30, width = 10, x = 0, y = 0 }
                        , scene = { height = 100, width = 100 }
                        , viewport = { height = 100, width = 100, x = 0, y = 0 }
                        }

                    viewport : Dom.Viewport
                    viewport =
                        { scene = { height = 100, width = 100 }
                        , viewport = { height = 100, width = 100, x = 0, y = 0 }
                        }

                    alignment =
                        Alignment.init { container = container, select = select, viewport = viewport }

                    expected =
                        [ Attrs.style "position" "fixed"
                        , Attrs.style "top" "40px"
                        , Attrs.style "left" "30px"
                        , Attrs.style "width" "10px"
                        ]
                in
                Expect.equal (Alignment.style (Just alignment)) expected
        ]
