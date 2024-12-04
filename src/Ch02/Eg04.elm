module Ch02.Eg04 exposing (main)

import Browser
import Browser.Events
import Float.Extra
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random exposing (Seed)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events
import Time


type alias Screen =
    { width : Float
    , height : Float
    , left : Float
    , right : Float
    , top : Float
    , bottom : Float
    }


screen : Screen
screen =
    let
        w =
            640

        h =
            240
    in
    { width = w
    , height = h
    , left = -w / 2
    , right = w / 2
    , top = -h / 2
    , bottom = h / 2
    }


initScreen : Float -> Float -> Screen
initScreen w h =
    { width = w
    , height = h
    , left = -w / 2
    , right = w / 2
    , top = -h / 2
    , bottom = h / 2
    }


expandScreenByRadius : Float -> Screen -> Screen
expandScreenByRadius radius s =
    let
        diameter =
            radius * 2

        ( w, h ) =
            ( s.width + diameter, s.height + diameter )
    in
    initScreen w h


shrinkScreenByRadius : Float -> Screen -> Screen
shrinkScreenByRadius radius s =
    expandScreenByRadius -radius s


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { ticks : Int
    , particles : List Particle
    , mouse : Vec
    , mouseDown : Bool
    , seed : Seed
    }


type alias Particle =
    { position : Vec
    , velocity : Vec
    , mass : Float
    }


particleRadius p =
    p.mass * 8


init : () -> ( Model, Cmd Msg )
init () =
    ( { ticks = 0
      , particles =
            [ Particle ( 0, screen.top + 30 ) ( 0, 0 ) 5
            ]
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
                , particles = List.map (updateParticle model.mouseDown) model.particles
              }
            , Cmd.none
            )


updateParticle : Bool -> Particle -> Particle
updateParticle mouseDown p_ =
    let
        p =
            p_ |> bounceWithinScreen screen

        newVelocity =
            vecAdd p.velocity (acceleration_ mouseDown screen p)
    in
    { p
        | velocity = newVelocity
        , position = vecAdd p.position newVelocity
    }


bounceWithinScreen : Screen -> Particle -> Particle
bounceWithinScreen s_ p =
    let
        s =
            shrinkScreenByRadius (particleRadius p) s_

        ( x, y ) =
            p.position

        ( dx, dy ) =
            p.velocity

        bounce =
            -0.9

        newVelocity =
            ( if (x < s.left && dx < 0) || (x > s.right && dx > 0) then
                dx * bounce

              else
                dx
            , if y > s.bottom && dy > 0 then
                dy * bounce

              else
                dy
            )

        newPosition =
            ( clamp s.left s.right x, y |> atMost s.bottom )
    in
    { p | position = newPosition, velocity = newVelocity }


atMost =
    min


acceleration_ isWindy s_ p =
    let
        s =
            shrinkScreenByRadius (particleRadius p) s_

        ( _, y ) =
            p.position

        friction =
            --The particle is touching the edge when it's within one pixel
            if y > s.bottom - 1 then
                let
                    coefficient =
                        0.1
                in
                vecScale -1 p.velocity
                    |> vecSetMag coefficient

            else
                ( 0, 0 )

        gravity =
            ( 0, 1 )

        windForce =
            if isWindy then
                ( 0.5, 0 )

            else
                ( 0, 0 )
    in
    vecAdd
        (windForce |> vecScale (1 / p.mass))
        (gravity |> vecScale (1 / p.mass))
        |> vecAdd
            (friction |> vecScale (1 / p.mass))


vecSetMag m v =
    v |> toPolar |> Tuple.mapFirst (always m) |> fromPolar


view : Model -> Html Msg
view model =
    Svg.svg
        [ SA.viewBox (viewBox4 screen.left screen.top screen.width screen.height)
        , style "display" "block"
        , style "width" "640"
        , style "fill" "none"
        , style "stroke" "none"
        , style "background" "#000"
        , style "shape-rendering" "geometric-precision"
        , style "stroke-linecap" "round"
        , Svg.Events.on "mousemove" mouseDecoder
        ]
        [ List.map viewParticle model.particles
            |> group []
        ]


viewParticle : Particle -> Svg Msg
viewParticle p =
    Svg.circle
        [ SA.r (String.fromFloat (particleRadius p))
        , style "fill" "#555"
        , style "stroke" "#fff"
        , style "stroke-width" "2"
        , translate p.position
        ]
        []


group =
    Svg.g


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
