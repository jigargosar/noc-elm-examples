module Example_1_9 exposing (main)

import Browser
import Html exposing (text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random exposing (Seed)
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


type alias Model =
    { ticks : Int
    , position : Vec
    , velocity : Vec
    , seed : Seed
    }


width =
    640


height =
    240


screen =
    { width = width
    , height = height
    , left = -width / 2
    , right = width / 2
    , top = -height / 2
    , bottom = height / 2
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { ticks = 0
      , position = ( 0, 0 )
      , velocity = ( 0, 0 )
      , seed = Random.initialSeed 0
      }
    , Cmd.none
    )


type Msg
    = Tick


subscriptions _ =
    Time.every (1000 / 60) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            let
                randomMagnitude =
                    Random.float 0 2

                randomAngle =
                    Random.float 0 (2 * pi)

                randomAcceleration =
                    Random.map2 (\mag ang -> fromPolar ( mag, ang )) randomMagnitude randomAngle

                ( acceleration, seed ) =
                    Random.step randomAcceleration model.seed

                maxSpeed =
                    5

                velocity =
                    vecAdd model.velocity acceleration
                        |> vecLimitMagnitude maxSpeed

                position =
                    vecAdd model.position velocity
                        |> warpPosition
            in
            ( { model
                | ticks = model.ticks + 1
                , position = position
                , velocity = velocity
                , seed = seed
              }
            , Cmd.none
            )


vecLimitMagnitude limit v =
    let
        ( mag, angle ) =
            toPolar v
    in
    ( atMost limit mag, angle )
        |> fromPolar


atMost =
    min


warpPosition ( x, y ) =
    ( if x > screen.right then
        screen.left

      else if x < screen.left then
        screen.right

      else
        x
    , if y > screen.bottom then
        screen.top

      else if y < screen.top then
        screen.bottom

      else
        y
    )


view : Model -> Html.Html Msg
view model =
    Svg.svg
        [ SA.viewBox (viewBox4 screen.left screen.top width height)
        , style "display" "block"
        , style "width" "640"
        , style "fill" "none"
        , style "stroke" "none"
        , style "background" "#000"
        , style "shape-rendering" "geometric-precision"
        , style "stroke-linecap" "round"
        ]
        [ Svg.circle
            [ SA.r "20"
            , style "fill" "#555"
            , style "stroke" "#fff"
            , style "stroke-width" "2"
            , translate model.position
            ]
            []
        ]


vecNormalize v =
    let
        ( _, angle ) =
            toPolar v

        unitVec =
            fromPolar ( 1, angle )
    in
    unitVec


viewVec v =
    polyline [ ( 0, 0 ), v ]


viewBox4 x y w h =
    [ x, y, w, h ] |> List.map String.fromFloat |> String.join " "


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


type alias Vec =
    ( Float, Float )


vecSub =
    map2 (-)


vecAdd =
    map2 (+)


vecScale s ( x, y ) =
    ( x * s, y * s )


map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )
