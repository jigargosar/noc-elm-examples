module Example_01 exposing (..)

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
    viewSample1


viewSample1 =
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
        (randomWalkerPoints |> List.map viewPoint)


pointDiameter =
    4


pointRadius =
    pointDiameter / 2


randomWalkerPoints : List Point
randomWalkerPoints =
    let
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

        offsetsToPoints : List Point -> List Point
        offsetsToPoints offsets =
            List.foldl
                (\offset ( prevPoint, acc ) ->
                    let
                        newPoint =
                            add2 prevPoint (vecScale pointDiameter offset)
                    in
                    ( newPoint, newPoint :: acc )
                )
                ( ( 0, 0 ), [] )
                offsets
                |> Tuple.second
                |> List.reverse

        gen =
            Random.list 2000 randomDirVec
                |> Random.map offsetsToPoints
    in
    stepWithInitialSeed gen 1



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
        [ SA.r (String.fromFloat pointRadius)
        , translate ( x, y )
        , style "fill" "none"
        , style "fill" "#fff"

        --, style "stroke" "#FFF"
        --, style "stroke-width" (String.fromFloat pointRadius)
        , style "opacity" "0.5"
        ]
        []


px f =
    String.fromFloat f ++ "px"


translate ( x, y ) =
    style "translate" (px x ++ " " ++ px y)


stepWithInitialSeed gen seed =
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
