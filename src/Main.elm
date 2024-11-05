module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (style)
import Random exposing (Generator)
import Svg
import Svg.Attributes as SA


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init () =
    ( {}, Cmd.none )


subscriptions _ =
    Sub.none


update msg model =
    ( model, Cmd.none )


view _ =
    Svg.svg
        [ SA.viewBox "-250 -250 500 500"
        , style "width" "500"
        , style "background" "#111"
        , style "fill" "none"
        , style "stroke" "none"
        ]
        (points |> List.map viewPoint)


type alias Point =
    ( Float, Float )


randomPoint : Generator Point
randomPoint =
    -- Random.constant ( 100, 0 )
    let
        gen =
            -- Random.float -250 250
            Random.float -200 200
    in
    Random.pair gen gen


points =
    -- [ ( 100, 0 ) ]
    Random.step (Random.list 1000 randomPoint) (Random.initialSeed 1)
        |> Tuple.first


viewPoint ( x, y ) =
    Svg.circle
        [ SA.r "10"
        , translate ( x, y )
        , style "fill" "#FFF"
        , style "opacity" "0.1"
        ]
        []


px f =
    String.fromFloat f ++ "px"


translate ( x, y ) =
    style "translate" (px x ++ " " ++ px y)
