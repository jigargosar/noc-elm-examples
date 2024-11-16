module Example_06 exposing (main)

import Browser
import Html.Attributes exposing (style)
import Random
import Simplex exposing (PermutationTable)
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
    { ticks : Int, permTable : PermutationTable }


init () =
    ( { ticks = 0, permTable = Simplex.permutationTableFromInt 0 }
    , Random.generate PermTable Simplex.permutationTableGenerator
        |> always Cmd.none
    )


type Msg
    = Tick
    | PermTable PermutationTable


subscriptions _ =
    Time.every (1000 / cfg.freq) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | ticks = model.ticks + 1 }, Cmd.none )

        PermTable permTable ->
            ( { model | permTable = permTable }, Cmd.none )


view model =
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
        (points model.permTable cfg.maxPoints model.ticks |> List.map viewPoint)


cfg =
    { maxPoints = 1000
    , noiseFactor = 0.004
    , noiseOffset = 10000
    , freq = 60
    }


points permTable maxPoints n =
    let
        ( s, e ) =
            ( n - maxPoints |> atLeast 0, n - 1 )
    in
    List.range s e
        |> List.map
            (\i ->
                ( toFloat i * cfg.noiseFactor
                , (toFloat i + cfg.noiseOffset) * cfg.noiseFactor
                )
                    |> tMap (noise1dNorm permTable)
                    |> Tuple.mapBoth (lerp -320 320) (lerp -120 120)
            )


noise1dNorm permTable n =
    norm -1 1 (Simplex.noise2d permTable 0 n)


tMap fn =
    Tuple.mapBoth fn fn


atLeast =
    max


atMost =
    min


rangeMap a b c_ d n =
    norm a b n |> lerp c_ d


norm a b n =
    if b - a == 0 then
        0

    else
        (n - a) / (b - a)


lerp a b n =
    a + (b - a) * n


viewPoint ( x, y ) =
    Svg.circle
        [ SA.r "10"
        , translate ( x, y )
        , SA.strokeWidth "1"
        , SA.stroke "#ddd"
        , SA.fill "#000"

        --, SA.opacity "0.5"
        ]
        []


translate ( x, y ) =
    style "translate" (px x ++ " " ++ px y)


px f =
    String.fromFloat f ++ "px"
