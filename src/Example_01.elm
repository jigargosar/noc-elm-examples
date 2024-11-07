module Example_01 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (style)
import List.Extra
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
    viewSample


viewSample =
    Svg.svg
        [ viewBoxCentered (tupleRepeat 500)
        , style "width" "500"
        , style "background" "#111"
        , style "fill" "none"
        , style "stroke" "none"
        , style "shape-rendering" "geometric-precision"

        --, style "shape-rendering" "crispEdges"
        --, style "shape-rendering" "optimizeSpeed"
        ]
        (randomWalkerPoints 100
            |> stepWithInitialSeed 1
            |> List.map viewPoint
        )


type alias Config =
    { radius : Float, scale : Float, opacity : Float }


ic : Config
ic =
    { radius = 5, scale = 5 * 1.5, opacity = 0.3 }



--pointRadius =
--    5
--
--
--pointScale =
--    pointRadius * 1.5
--
--
--pointOpacity =
--    0.3


randomWalkerPoints : Int -> Generator (List Point)
randomWalkerPoints ct =
    Random.list ct randomDirVec
        |> Random.map (offsetsToPoints)


offsetsToPoints : List Point -> List Point
offsetsToPoints offsets =
    offsets
        |> List.Extra.scanl add2 ( 0, 0 )
        |> List.map (vecScale ic.scale)


randomDirVec : Generator Point
randomDirVec =
    Random.uniform ( 1, 0 )
        [ ( 0, 1 )
        , ( 0, -1 )
        , ( -1, 0 )

        --, ( -1, 1 )
        --, ( 1, -1 )
        --, ( 1, 1 )
        --, ( -1, -1 )
        ]



--points =
--    let
--        randomPoint : Generator Point
--        randomPoint =
--            -- Random.constant ( 100, 0 )
--            let
--                gen =
--                    -- Random.float -250 250
--                    Random.float -200 200
--            in
--            Random.pair gen gen
--    in
--    -- [ ( 100, 0 ) ]
--    Random.step (Random.list 1000 randomPoint) (Random.initialSeed 1)
--        |> Tuple.first
--
--
--
--viewPoint2 ( x, y ) =
--    Svg.rect
--        [ SA.width "2"
--        , SA.height "2"
--
--        --, SA.x "1"
--        --, SA.y "1"
--        --, SA.x "-5"
--        --, SA.y "-5"
--        --, style "fill" "#FFF"
--        , style "stroke-linecap" "round"
--        , style "stroke-linejoin" "round"
--        , style "fill" "none"
--        , style "stroke" "#FFF"
--        , translate ( x, y )
--        , style "stroke-width" "1"
--
--        --, style "opacity" "0.5"
--        ]
--        []


viewPoint ( x, y ) =
    Svg.circle
        [ SA.r (String.fromFloat ic.radius)
        , translate ( x, y )
        , style "fill" "none"
        , style "fill" "#fff"

        --, style "stroke" "#FFF"
        --, style "stroke-width" (String.fromFloat pointRadius)
        , opacity ic.opacity
        ]
        []


opacity n =
    style "opacity" (String.fromFloat n)


px f =
    String.fromFloat f ++ "px"


translate ( x, y ) =
    style "translate" (px x ++ " " ++ px y)


stepWithInitialSeed seed gen =
    Random.step gen (Random.initialSeed seed) |> Tuple.first


vecScale n ( x, y ) =
    ( x * n, y * n )


curry fn ( a, b ) =
    fn a b


add2 ( a, b ) ( c, d ) =
    ( a + c, b + d )


tupleRepeat x =
    ( x, x )


viewBoxCentered ( w, h ) =
    SA.viewBox ([ w / -2, h / -2, w, h ] |> List.map String.fromFloat |> String.join " ")


type alias Point =
    ( Float, Float )
