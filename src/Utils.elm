module Utils exposing (..)

import Html.Attributes exposing (style)
import Svg
import Svg.Attributes as SA


type alias Vec =
    ( Float, Float )


type alias Screen =
    { width : Float
    , height : Float
    , left : Float
    , right : Float
    , top : Float
    , bottom : Float
    }


screen : Screen
screen =
    let
        w =
            640

        h =
            240
    in
    { width = w
    , height = h
    , left = -w / 2
    , right = w / 2
    , top = -h / 2
    , bottom = h / 2
    }


initScreen : Float -> Float -> Screen
initScreen w h =
    { width = w
    , height = h
    , left = -w / 2
    , right = w / 2
    , top = -h / 2
    , bottom = h / 2
    }


expandScreenByRadius : Float -> Screen -> Screen
expandScreenByRadius radius s =
    let
        diameter =
            radius * 2

        ( w, h ) =
            ( s.width + diameter, s.height + diameter )
    in
    initScreen w h


shrinkScreenByRadius : Float -> Screen -> Screen
shrinkScreenByRadius radius s =
    expandScreenByRadius -radius s


svg attrs =
    Svg.svg
        ([ SA.viewBox "-320 -120 640 240"
         , style "display" "block"
         , style "width" "640"
         , style "fill" "none"
         , style "stroke" "none"
         , style "background" "#000"
         , style "shape-rendering" "geometric-precision"
         ]
            ++ attrs
        )


fill =
    style "fill"


circle r attrs =
    Svg.circle (SA.r (String.fromFloat r) :: attrs) []


translate ( x, y ) =
    style "translate" (px x ++ " " ++ px y)


px f =
    String.fromFloat f ++ "px"
