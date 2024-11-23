module Example_2_1 exposing (main)

import Browser
import Browser.Events
import Float.Extra
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
    , mouse : Vec
    , mouseDown : Bool
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
      , position = ( 0, screen.top + 30 )
      , velocity = ( 0, 0 )
      , mouse = ( 0, 0 )
      , mouseDown = False
      , seed = Random.initialSeed 0
      }
    , Cmd.none
    )


type Msg
    = Tick
    | MouseMoved Vec
    | MouseDown Bool


subscriptions _ =
    [ Time.every (1000 / 60) (always Tick)
    , Browser.Events.onMouseDown (JD.succeed (MouseDown True))
    , Browser.Events.onMouseUp (JD.succeed (MouseDown False))
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown bool ->
            ( { model | mouseDown = bool }, Cmd.none )

        MouseMoved vec ->
            ( { model | mouse = vec |> vecAdd ( screen.left, screen.top ) }, Cmd.none )

        Tick ->
            ( { model
                | ticks = model.ticks + 1
                , position = vecAdd model.position model.velocity
                , velocity =
                    vecAdd
                        (velocity_ model.position model.velocity)
                        (acceleration_ model.mouseDown)
              }
            , Cmd.none
            )


velocity_ position velocity =
    let
        s =
            screen

        ( x, y ) =
            position

        ( dx, dy ) =
            velocity
    in
    ( if (x < s.left && dx < 0) || (x > s.right && dx > 0) then
        -dx

      else
        dx
    , if (y < s.top && dy < 0) || (y > s.bottom && dy > 0) then
        -dy

      else
        dy
    )


acceleration_ mouseDown =
    let
        gravity =
            ( 0, 0.1 )

        windForce =
            if mouseDown then
                ( 0.1, 0 )

            else
                ( 0, 0 )

        acceleration =
            vecAdd windForce gravity
    in
    acceleration


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
        , Svg.Events.on "mousemove" mouseDecoder
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


mouseDecoder =
    JD.map2 Tuple.pair
        (JD.field "x" JD.float)
        (JD.field "y" JD.float)
        |> JD.map MouseMoved


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


distanceSquaredFromTo from to =
    let
        ( x, y ) =
            vecFromTo from to
    in
    add (mul x x) (mul y y)


add =
    (+)


mul =
    (*)


vecFromTo from to =
    vecSub to from


angleFromTo from to =
    let
        ( x, y ) =
            vecFromTo from to
    in
    atan2 y x


vecAdd =
    map2 (+)


vecScale s ( x, y ) =
    ( x * s, y * s )


map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )
