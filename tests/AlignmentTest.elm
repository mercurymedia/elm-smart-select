module AlignmentTest exposing (suite)

import Browser.Dom as Dom exposing (Element)
import Expect
import SmartSelect.Alignment as Alignment
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Alignment"
        [ test "Should return Above when the container element does not fit below the select element" <|
            \_ ->
                let
                    select : Element
                    select =
                        { element = { height = 34, width = 502, x = 428, y = 480 }
                        , scene = { height = 728, width = 1272 }
                        , viewport = { height = 632, width = 1272, x = 0, y = 0 }
                        }

                    container : Element
                    container =
                        { element = { height = 331, width = 502, x = 428, y = 150 }
                        , scene = { height = 728, width = 1272 }
                        , viewport = { height = 632, width = 1272, x = 0, y = 0 }
                        }

                    viewport : Dom.Viewport
                    viewport =
                        { scene = { height = 728, width = 1272 }
                        , viewport = { height = 632, width = 1272, x = 0, y = 0 }
                        }

                    alignment =
                        Alignment.init (Alignment.params { container = container, select = select, viewport = viewport })
                in
                Expect.equal (Alignment.isAbove alignment) True
        , test "Should return Below when the container element fit below the select element" <|
            \_ ->
                let
                    select : Element
                    select =
                        { element = { height = 34, width = 502, x = 428, y = 480 }
                        , scene = { height = 1073, width = 1272 }
                        , viewport = { height = 977, width = 1272, x = 0, y = 0 }
                        }

                    container : Element
                    container =
                        { element = { height = 282, width = 502, x = 428, y = 513 }
                        , scene = { height = 1073, width = 1272 }
                        , viewport = { height = 977, width = 1272, x = 0, y = 0 }
                        }

                    viewport : Dom.Viewport
                    viewport =
                        { scene = { height = 728, width = 1272 }
                        , viewport = { height = 632, width = 1272, x = 0, y = 0 }
                        }

                    alignment =
                        Alignment.init (Alignment.params { container = container, select = select, viewport = viewport })
                in
                Expect.notEqual (Alignment.isAbove alignment) True
        ]
