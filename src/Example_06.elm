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
    )


type Msg
    = Tick
    | PermTable PermutationTable


subscriptions _ =
    Time.every (1000 / 60) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | ticks = model.ticks + 1 }, Cmd.none )

        PermTable permutationTable ->
            ( { model | permTable = permutationTable }, Cmd.none )


view model =
    Svg.svg
        [ SA.viewBox "-320 -120 640 240"
        , style "display" "block"
        , style "width" "640"
        , style "fill" "none"
        , style "stroke" "none"
        ]
        --(points model.ticks |> List.map viewPoint)
        (points model.permTable model.ticks |> List.map viewPoint)


points permTable n =
    List.range 0 (n - 1)
        |> List.map
            (toFloat
                >> (\i ->
                        let
                            dx =
                                i * 0.004

                            dy =
                                (i + 10000) * 0.004
                        in
                        ( Simplex.noise2d permTable dx 0
                        , Simplex.noise2d permTable dy 0
                        )
                    --( Simplex.noise2d permTable dx 0
                    --, Simplex.noise2d permTable2 dy 0
                    --)
                   )
            )
        --|> Debug.log "pt"
        |> List.map (Tuple.mapBoth (rangeMap -1 1 -320 320) (rangeMap -1 1 -120 120))


rangeMap a b c d n =
    norm a b n |> lerp c d


norm a b n =
    if b - a == 0 then
        0

    else
        (n - a) / (b - a)


lerp a b n =
    a + (b - a) * n



--permTable : PermutationTable
--permTable =
--    Simplex.permutationTableFromInt 0
--
--
--permTable2 : PermutationTable
--permTable2 =
--    Simplex.permutationTableFromInt 100000
--


viewPoint ( x, y ) =
    Svg.circle
        [ SA.r "24"
        , translate ( x, y )
        , SA.strokeWidth "2"
        , SA.stroke "#fff"
        , SA.fill "#000"
        ]
        []


translate ( x, y ) =
    style "translate" (px x ++ " " ++ px y)


px f =
    String.fromFloat f ++ "px"
