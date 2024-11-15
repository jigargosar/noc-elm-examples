module Example_05 exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (style)
import List.Extra
import Random
import Svg
import Svg.Attributes as SA exposing (fill, stroke, strokeWidth)
import Time


main =
    Browser.element
        { init = \() -> ( { ticks = 0 }, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { ticks : Int }


type Msg
    = Tick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 144) (always Tick)


update msg model =
    case msg of
        Tick ->
            ( { model | ticks = model.ticks + 1 }, Cmd.none )


view model =
    Svg.svg
        [ SA.viewBox "-320 -120 640 240"
        , style "width" "640"
        , fill "none"
        , stroke "none"
        , style "display" "block"
        ]
        (Random.list model.ticks randomIdx
            |> Random.map List.Extra.frequencies
            |> stepWithInitialSeed 1
            |> List.map viewBar
        )


stepWithInitialSeed seed gen =
    Random.step gen (Random.initialSeed seed) |> Tuple.first


totalBars =
    20


barWidth =
    640 / totalBars


randomIdx =
    let
        toIdx : Float -> Int
        toIdx f =
            f * toFloat totalBars |> floor
    in
    randomAcceptRejectProbability |> Random.map toIdx


randomAcceptRejectProbability =
    let
        randomProbability =
            Random.float 0 1
    in
    Random.pair randomProbability randomProbability
        |> Random.andThen
            (\( a, b ) ->
                if b < a then
                    Random.constant a

                else
                    Random.lazy (always randomAcceptRejectProbability)
            )



--|> Random.map toIdx


viewBar ( i, val ) =
    let
        h =
            val |> toFloat
    in
    Svg.rect
        [ SA.width (String.fromFloat barWidth)
        , SA.height (String.fromFloat h)
        , translate ( toFloat i * barWidth - 320, -h + 120 )
        , fill "#000"
        , stroke "#FFF"
        , strokeWidth "2"
        ]
        []


translate ( x, y ) =
    style "translate" (String.fromFloat x ++ "px " ++ String.fromFloat y ++ "px")
