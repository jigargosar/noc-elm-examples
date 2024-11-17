module Example_1_3 exposing (main)

import Browser
import Html exposing (text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Svg
import Svg.Attributes as SA
import Svg.Events
import Time


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Vec =
    ( Float, Float )


type alias Model =
    { ticks : Int
    , mouse : Vec
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { ticks = 0
      , mouse = ( 200, -40 )
      }
    , Cmd.none
    )


type Msg
    = Tick
    | MouseMoved Vec


subscriptions _ =
    Time.every (1000 / 60) (always Tick)
        |> always Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | ticks = model.ticks + 1 }, Cmd.none )

        MouseMoved vec ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        mouse =
            model.mouse

        bottomRight =
            ( 320, 120 )
    in
    Svg.svg
        [ SA.viewBox "-320 -120 640 240"
        , style "display" "block"
        , style "width" "640"
        , style "fill" "none"
        , style "stroke" "none"
        , style "background" "#000"
        , style "shape-rendering" "geometric-precision"
        , Svg.Events.on "mousemove"
            (JD.map2 Tuple.pair
                (JD.field "x" JD.float)
                (JD.field "y" JD.float)
                |> JD.map MouseMoved
            )

        --, style "shape-rendering" "optimizeSpeed"
        --, style "shape-rendering" "crispEdges"
        ]
        [ viewVec mouse
            [ style "stroke" "#fff"
            , SA.strokeWidth "4"
            , SA.opacity "0.2"
            ]
        , viewVec bottomRight
            [ style "stroke" "#fff"
            , SA.strokeWidth "4"
            , SA.opacity "0.2"
            ]
        , viewVec (vecSub bottomRight mouse)
            [ style "stroke" "#fff"
            , SA.strokeWidth "4"
            , SA.opacity "1"
            , translate mouse
            ]
        ]


viewVec v =
    polyline [ ( 0, 0 ), v ]


px f =
    String.fromFloat f ++ "px"


translate ( x, y ) =
    style "translate" (px x ++ " " ++ px y)


polyline pts attrs =
    Svg.polyline (SA.points (toPointsAttribute pts) :: attrs) []


toPointsAttribute : List Vec -> String
toPointsAttribute pts =
    let
        ptToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    pts |> List.map ptToString |> String.join " "


vecSub =
    map2 (-)


vecAdd =
    map2 (+)


map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )
