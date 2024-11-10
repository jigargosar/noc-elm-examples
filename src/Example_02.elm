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
    ( Model 850, Cmd.none )


type Msg
    = OnTick


subscriptions : Model -> Sub Msg
subscriptions model =
    if barHeightLimitReached model.ticks then
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
        (barFrequencies model.ticks
            |> List.map (curry viewBar)
        )


barHeightLimitReached ticks =
    barFrequencies ticks |> List.any maxBarFreqReached


barFrequencies ticks =
    (ic.ticksScale * ticks)
        |> randomBars
        |> stepWithInitialSeed ic.seed


randomBarIndex =
    Random.int 0 (ic.rectCount - 1)


randomBars ct =
    Random.list ct randomBarIndex
        |> Random.map List.Extra.frequencies


type alias Config =
    { seed : Int
    , rectCount : Int
    , width : Float
    , height : Float
    , ticksScale : Int
    }


ic : Config
ic =
    { seed = 16
    , rectCount = 80
    , width = 500
    , height = 500
    , ticksScale = 5
    }


maxBarFreqReached : ( a, Int ) -> Bool
maxBarFreqReached ( _, freq ) =
    freq > maxBarFreq


maxBarFreq : Int
maxBarFreq =
    round (ic.height / 2)


barY : Int -> Float
barY freq =
    (ic.height / 2) - toFloat freq


barX idx =
    toFloat idx * barWidth ic - (ic.width / 2)


barWidth c =
    c.width / toFloat c.rectCount


viewBar : Int -> Int -> Svg.Svg msg
viewBar idx freq =
    Svg.rect
        [ SA.width (px (barWidth ic))
        , SA.height (px (toFloat freq))
        , translate ( barX idx, barY freq )
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
