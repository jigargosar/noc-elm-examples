module Example_1_7 exposing (main)

import Browser
import Html exposing (text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random
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
    , mouse : Vec
    , position : Vec
    , velocity : Vec
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
      , mouse = ( screen.left, screen.top )
      , position = ( 0, 0 )
      , velocity = ( 2, 2 )
      }
    , Random.generate identity <|
        Random.map2 GotPositionAnVelocity randomVecOnScreen randomVelocity
    )


randomVelocity =
    Random.pair (Random.float -2 2) (Random.float -2 2)


randomVecOnScreen =
    Random.pair (Random.float screen.left screen.right)
        (Random.float screen.top screen.bottom)


type Msg
    = Tick
    | MouseMoved Vec
    | GotPositionAnVelocity Vec Vec


subscriptions _ =
    Time.every (1000 / 60) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model
                | ticks = model.ticks + 1
                , position =
                    vecAdd model.position model.velocity
                        |> warpPosition
              }
            , Cmd.none
            )

        MouseMoved vec ->
            ( { model | mouse = vec }, Cmd.none )

        GotPositionAnVelocity pos vel ->
            ( { model | position = pos, velocity = vel }, Cmd.none )


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
    let
        mouse =
            model.mouse |> vecAdd ( screen.left, screen.top )
    in
    Svg.svg
        [ SA.viewBox (viewBox4 screen.left screen.top width height)
        , style "display" "block"
        , style "width" "640"
        , style "fill" "none"
        , style "stroke" "none"
        , style "background" "#000"
        , style "shape-rendering" "geometric-precision"
        , Svg.Events.on "mousemove" mouseDecoder
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


mouseDecoder =
    JD.map2 Tuple.pair
        (JD.field "x" JD.float)
        (JD.field "y" JD.float)
        |> JD.map MouseMoved


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
