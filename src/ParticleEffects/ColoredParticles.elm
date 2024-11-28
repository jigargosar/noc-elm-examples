module ParticleEffects.ColoredParticles exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random exposing (Generator, Seed)
import Svg
import Svg.Attributes as SA
import Time


type alias Screen =
    { width : Float
    , height : Float
    , left : Float
    , right : Float
    , top : Float
    , bottom : Float
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
expandScreenByRadius radius screen =
    let
        diameter =
            radius * 2

        ( w, h ) =
            ( screen.width + diameter, screen.height + diameter )
    in
    initScreen w h


shrinkScreenByRadius : Float -> Screen -> Screen
shrinkScreenByRadius radius screen =
    expandScreenByRadius -radius screen


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { particles : List Particle
    , screen : Screen
    , seed : Seed
    }


type alias Particle =
    { position : Vec
    , radius : Float
    , velocity : Vec
    }


type alias Vec =
    ( Float, Float )


vecAdd =
    map2 (+)


map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


init : () -> ( Model, Cmd Msg )
init () =
    ( Model [] (initScreen 1000 500) (Random.initialSeed 0)
    , Random.generate GotSeed Random.independentSeed
    )


type Msg
    = Tick
    | GotSeed Seed


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Time.every (1000 / 60) (always Tick) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            let
                updateParticle p =
                    { p | position = vecAdd p.position p.velocity }
                        |> bounceParticle model.screen
            in
            ( { model | particles = List.map updateParticle model.particles }, Cmd.none )

        GotSeed initialSeed ->
            let
                randomParticles =
                    randomParticle model.screen
                        |> Random.list 200
                        |> Random.map (List.sortBy .radius)

                ( particles, seed ) =
                    Random.step randomParticles initialSeed
            in
            ( { model | particles = particles, seed = seed }, Cmd.none )


bounceParticle screen_ particle =
    let
        screen =
            shrinkScreenByRadius particle.radius screen_

        ( x, y ) =
            particle.position

        ( dx, dy ) =
            particle.velocity

        velocity =
            ( if x < screen.left && dx < 0 then
                -dx

              else if x > screen.right && dx > 0 then
                -dx

              else
                dx
            , if y < screen.top && dy < 0 then
                -dy

              else if y > screen.bottom && dy > 0 then
                -dy

              else
                dy
            )
    in
    { particle | velocity = velocity }


randomParticle : Screen -> Generator Particle
randomParticle screen =
    let
        randomRadius =
            Random.float 5 40

        randomVelocity =
            Random.pair (Random.float -1 1) (Random.float -1 1)

        randomPosition radius =
            randomPointInScreen (shrinkScreenByRadius radius screen)
    in
    randomRadius
        |> Random.andThen
            (\radius ->
                Random.map3 Particle
                    (randomPosition radius)
                    (Random.constant radius)
                    randomVelocity
            )


randomPointInScreen screen =
    Random.pair (Random.float screen.left screen.right) (Random.float screen.top screen.bottom)


view : Model -> Html Msg
view model =
    let
        screen =
            model.screen
    in
    Svg.svg
        [ viewBoxFromScreen screen
        , style "width" (px screen.width)
        , style "height" (px screen.height)
        , style "display" "block"
        , style "background" "#111"
        , SA.fill "none"
        , SA.stroke "none"
        ]
        [ Svg.defs []
            [ Svg.linearGradient
                [ SA.id "grad"
                , SA.gradientUnits "userSpaceOnUse"

                --, SA.x1 "-50%"
                --, SA.y1 "-50%"
                --, SA.x2 "50%"
                --, SA.y2 "50%"
                --, SA.x1 "0%"
                --, SA.y1 "0%"
                --, SA.x2 "100%"
                --, SA.y2 "100%"
                --, SA.x1 "-0.5"
                --, SA.y1 "-0.5"
                --, SA.x2 "0.5"
                --, SA.y2 "0.5"
                --
                --, SA.width "100%"
                --, SA.height "100%"
                --, translate ( screen.left, screen.top )
                --, style "rotate" "180deg"
                --, style "transform" "translate(-500px, -250px) rotate(45deg) "
                , style "transform" "rotate(45deg) translate(-500px, -250px) "

                --, style "transform" "rotate(315deg) translate(-500px, -250px) "
                --, style "transform" "rotate(-45deg) translate(-500px, -250px) "
                ]
                [ Svg.stop [ SA.offset "50%", SA.stopColor "white" ] []

                --, Svg.stop [ SA.offset "50%", SA.stopColor "magenta" ] []
                , Svg.stop [ SA.offset "50%", SA.stopColor "blue" ] []
                ]
            ]
        , model.particles
            |> List.map (viewParticle screen)
            |> group []
        , Svg.circle [ SA.r "5", SA.fill "dodgerblue" ] []
        ]


group =
    Svg.g


viewParticle : Screen -> Particle -> Html Msg
viewParticle scr particle =
    let
        ( x, y ) =
            particle.position

        hue =
            (360 / scr.width) * (x + scr.left)
    in
    circle particle.radius
        --[ translate ( x, y )
        [ SA.cx (String.fromFloat x)
        , SA.cy (String.fromFloat y)
        , SA.stroke "#000"
        , SA.strokeWidth "1"

        --, SA.fill ("hsl(" ++ String.fromFloat hue ++ "deg 100 50/1)")
        , SA.fill "url(#grad)"
        ]


viewBoxFromScreen s =
    [ s.left, s.top, s.width, s.height ]
        |> List.map String.fromFloat
        |> String.join " "
        |> SA.viewBox


circle r attrs =
    Svg.circle (SA.r (px r) :: attrs) []


translate ( x, y ) =
    style "translate" (px x ++ " " ++ px y)


px f =
    String.fromFloat f ++ "px"
