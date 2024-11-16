module Example_1_1 exposing (main)

import Html
import Html.Attributes exposing (style)
import Svg
import Svg.Attributes as SA


main =
    view ()


view _ =
    Svg.svg
        [ SA.viewBox "-320 -120 640 240"
        , style "display" "block"
        , style "width" "640"
        , style "fill" "none"
        , style "stroke" "none"
        , style "background" "#000"
        , style "shape-rendering" "geometric-precision"

        --, style "shape-rendering" "optimizeSpeed"
        --, style "shape-rendering" "crispEdges"
        ]
        [ Svg.circle
            [ SA.r "20"
            , style "fill" "#444"
            , style "stroke" "#fff"
            , SA.strokeWidth "6"
            ]
            []
        ]
