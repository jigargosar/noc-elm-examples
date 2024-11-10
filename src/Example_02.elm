module Example_02 exposing (..)

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
        ticksPerSecond 60 OnTick


ticksPerSecond n msg =
    Time.every (1000 / n) (always msg)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        OnTick ->
            ( { model | ticks = model.ticks + 1 }, Cmd.none )


view : Model -> Html msg
view model =
    Svg.svg
        [ viewBoxCentered (tupleRepeat ic.width)
        , style "width" (px ic.width)
        , style "height" (px ic.height)
        , style "background" "#111"
        , style "fill" "none"
        , style "stroke" "none"
        , style "shape-rendering" "geometric-precision"
        ]
        (stepWithInitialSeed ic.seed (randomBars (5 * model.ticks))
            |> List.map (curry viewBar)
        )


randomBarIndex =
    Random.int 0 (ic.rectCount - 1)


randomBars ct =
    Random.list ct randomBarIndex
        |> Random.map List.Extra.frequencies


type alias Config =
    { seed : Int
    , limit : Int
    , rectCount : Int
    , width : Float
    , height : Float

    --, radius : Float
    --, scale : Float
    --, opacity : Float
    }


ic : Config
ic =
    { seed = 16
    , limit = 2000
    , rectCount = 20
    , width = 500
    , height = 500

    --, radius = 5
    --, scale = 5 * 1.5
    --, opacity = 0.2
    }


barWidth c =
    ic.width / toFloat c.rectCount


viewBar : Int -> Int -> Svg.Svg msg
viewBar i n =
    Svg.rect
        [ SA.width (px (barWidth ic))
        , SA.height (px (toFloat n))
        , translate ( toFloat i * barWidth ic - (ic.width / 2), (ic.height / 2) - toFloat n )
        , style "fill" "#000"
        , style "stroke" "#fff"
        , style "stroke-width" "2"
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
