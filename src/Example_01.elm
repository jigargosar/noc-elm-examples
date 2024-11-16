module Example_01 exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as HA exposing (style)
import List.Extra
import Random exposing (Generator)
import Svg
import Svg.Attributes as SA
import Time


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { ticks : Int
    }


init : () -> ( Model, Cmd msg )
init () =
    ( Model 0, Cmd.none )


type Msg
    = OnTick


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.ticks >= ic.limit then
        Sub.none

    else
        Time.every (1000 / 60) (always OnTick)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        OnTick ->
            ( { model | ticks = model.ticks + 1 }, Cmd.none )


view : Model -> Html msg
view model =
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
        (randomWalkerPoints model.ticks
            |> stepWithInitialSeed ic.seed
            |> List.map viewPoint
        )


type alias Config =
    { seed : Int
    , limit : Int
    , radius : Float
    , scale : Float
    , opacity : Float
    }


ic : Config
ic =
    { seed = 16
    , limit = 2000
    , radius = 5
    , scale = 5 * 1.5
    , opacity = 0.2
    }


randomWalkerPoints : Int -> Generator (List Point)
randomWalkerPoints ct =
    Random.list ct randomDirVec
        |> Random.map offsetsToPoints


offsetsToPoints : List Point -> List Point
offsetsToPoints offsets =
    offsets
        |> List.Extra.scanr add2 originPt
        |> List.map (vecScale ic.scale)


originPt =
    tupleRepeat 0


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
