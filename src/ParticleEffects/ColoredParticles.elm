module ParticleEffects.ColoredParticles exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import List.Extra
import Random exposing (Generator, Seed)
import Svg exposing (Attribute)
import Svg.Attributes as SA
import Time
import Utils exposing (..)


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
            Random.float 5 15

        randomVelocity =
            Random.pair (Random.float -0.5 0.5) (Random.float -0.5 0.5)

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
                , style "transform" "rotate(45deg) translate(-500px, -250px) "
                ]
                [ Svg.stop [ SA.offset "0", SA.stopColor "white" ] []
                , Svg.stop [ SA.offset "0.5", SA.stopColor "magenta" ] []
                , Svg.stop [ SA.offset "1", SA.stopColor "blue" ] []
                ]
            ]
        , model.particles
            |> List.Extra.selectSplit
            |> List.concatMap (\( _, p, rest ) -> viewParticleConnections p rest)
            |> group []
        , model.particles
            |> List.map (viewParticle screen)
            |> group []
        ]


attrPoints : List Vec -> Attribute Msg
attrPoints list =
    list
        |> List.map
            (\( x, y ) ->
                [ x, y ]
                    |> List.map String.fromFloat
                    |> String.join ","
            )
        |> String.join " "
        |> SA.points


viewParticleConnections : Particle -> List Particle -> List (Html Msg)
viewParticleConnections a bs =
    let
        maxDistance =
            100

        renderConnection distance b =
            Svg.polyline
                [ [ a, b ]
                    |> List.map .position
                    |> attrPoints
                , SA.stroke "#fff"
                , style "opacity" (String.fromFloat (1 - (distance / maxDistance)))
                ]
                []
    in
    bs
        |> List.filterMap
            (\b ->
                let
                    distance =
                        vecDistanceFromTo a.position b.position
                in
                if distance < maxDistance then
                    Just (renderConnection distance b)

                else
                    Nothing
            )


vecFromTo a b =
    vecSub b a


vecLengthSquared ( x, y ) =
    add (mul x x) (mul y y)


vecLength v =
    vecLengthSquared v |> sqrt


vecDistanceFromTo a b =
    vecFromTo a b
        |> vecLength


group =
    Svg.g


viewParticle : Screen -> Particle -> Html Msg
viewParticle _ particle =
    let
        ( x, y ) =
            particle.position
    in
    circle particle.radius
        [ SA.cx (String.fromFloat x)
        , SA.cy (String.fromFloat y)
        , SA.stroke "#000"
        , SA.strokeWidth "1"
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
