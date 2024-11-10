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
    , config : Config
    }


init : () -> ( Model, Cmd msg )
init () =
    let
        initialConfig : Config
        initialConfig =
            { seed = 16
            , totalBars = 80
            , width = 500
            , height = 500
            }
    in
    ( Model 0 initialConfig, Cmd.none )


type Msg
    = OnTick


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        c =
            model.config

        maxFreq =
            round (c.height * 0.75)

        surpassedMaxFreq ( _, freq ) =
            freq > maxFreq
    in
    if frequencies model |> List.any surpassedMaxFreq then
        Sub.none

    else
        Time.every (1000 / 60) (always OnTick)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        OnTick ->
            let
                scaledElapsedTicks =
                    round (toFloat model.config.totalBars * 0.75)
            in
            ( { model | ticks = model.ticks + scaledElapsedTicks }, Cmd.none )


view : Model -> Html msg
view model =
    let
        c =
            model.config
    in
    Svg.svg
        [ viewBoxCentered (tupleRepeat c.width)
        , style "width" (px c.width)
        , style "height" (px c.height)
        , style "background" "#111"
        , style "fill" "none"
        , style "stroke" "none"
        , style "shape-rendering" "geometric-precision"
        ]
        (frequencies model
            |> List.map (curry (viewBar c))
        )


frequencies model =
    let
        c =
            model.config

        randomIdx =
            Random.int 0 (c.totalBars - 1)
    in
    (Random.list model.ticks randomIdx
        |> Random.map List.Extra.frequencies
    )
        |> stepWithInitialSeed c.seed


type alias Config =
    { seed : Int
    , totalBars : Int
    , width : Float
    , height : Float
    }


barY c freq =
    (c.height / 2) - toFloat freq


barX c idx =
    toFloat idx * barWidth c - (c.width / 2)


barWidth c =
    c.width / toFloat c.totalBars


viewBar : Config -> Int -> Int -> Svg.Svg msg
viewBar c idx freq =
    Svg.rect
        [ width (barWidth c)
        , height (toFloat freq)
        , translate ( barX c idx, barY c freq )
        , style "fill" "#000"
        , style "stroke" "#fff"
        , style "stroke-width" "2"
        ]
        []


width n =
    style "width" (String.fromFloat n)


height n =
    style "height" (String.fromFloat n)


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
