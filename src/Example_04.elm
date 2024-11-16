module Example_04 exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as HA exposing (style)
import List.Extra
import Random exposing (Generator)
import Random.Float
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
subscriptions _ =
    Time.every (1000 / 240) (always OnTick)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        OnTick ->
            ( { model | ticks = model.ticks + 1 }, Cmd.none )


view : Model -> Html msg
view model =
    Svg.svg
        [ viewBoxCentered ( 640, 240 )
        , style "width" "640"
        , style "display" "block"
        , style "background" "#111"
        , style "fill" "none"
        , style "stroke" "none"
        , style "shape-rendering" "geometric-precision"
        ]
        --(Random.list 1000 randomGaussianPoint
            (Random.list model.ticks randomGaussianPoint
            |> stepWithInitialSeed 1
            |> List.map viewPoint
        )


randomGaussianPoint : Generator Point
randomGaussianPoint =
    Random.Float.normal 0 90
        |> Random.map (\x -> ( x, 0 ))


viewPoint ( x, y ) =
    Svg.circle
        [ SA.r "8"
        , translate ( x, y )
        , style "fill" "none"
        , style "fill" "#fff"
        , opacity (2/100)
        ]
        []



-- HELPERS


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
