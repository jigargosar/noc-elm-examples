module Ch02.Eg05 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random exposing (Generator, Seed)
import Random.Extra
import Svg exposing (text)
import Svg.Attributes as SA
import Svg.Events as SE
import Time
import Utils exposing (..)


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
    , seed : Seed
    }


type alias Particle =
    { position : Vec
    , velocity : Vec
    , mass : Float
    }


initParticle total i mass =
    let
        gap =
            screen.width / toFloat total

        firstParticleX =
            screen.left + (gap / 2)

        x =
            firstParticleX + (toFloat i * gap)
    in
    { position = ( x, screen.top + 30 ), velocity = ( 0, 0 ), mass = mass }


randomParticle total i =
    Random.float 0.5 3
        |> Random.map (initParticle total i)


randomParticles : Int -> Random.Generator (List Particle)
randomParticles total =
    List.range 0 (total - 1)
        |> List.map (randomParticle total)
        |> Random.Extra.combine


randomResetParticles : Generator (List Particle)
randomResetParticles =
    randomParticles 10


init : () -> ( Model, Cmd msg )
init () =
    let
        ( particles, seed ) =
            Random.step randomResetParticles (Random.initialSeed 0)
    in
    ( { particles = particles, seed = seed }
    , Cmd.none
    )


type Msg
    = Tick
    | Reset


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | particles = List.map updateParticle model.particles }, Cmd.none )

        Reset ->
            let
                ( particles, seed ) =
                    Random.step randomResetParticles model.seed
            in
            ( { model | particles = particles, seed = seed }, Cmd.none )


updateParticle p =
    let
        velocity =
            vecAdd p.velocity (particleAcceleration p)
    in
    { p
        | velocity = velocity
        , position = vecAdd p.position velocity
    }
        |> checkEdges


checkEdges p =
    let
        ( x, y ) =
            p.position

        ( vx, vy ) =
            p.velocity

        s =
            --screen
            shrinkScreenByRadius (particleRadius p) screen

        nvy =
            if
                y > s.bottom
                --&& vy > 0
            then
                vy * -0.9

            else
                vy
    in
    { p | position = ( x, y |> atMost s.bottom ), velocity = ( vx, nvy ) }


atMost =
    min


particleAcceleration : Particle -> Vec
particleAcceleration p =
    let
        gravity =
            ( 0, 0.1 )

        ( _, y ) =
            p.position

        drag =
            if y + particleRadius p > 0 then
                p.velocity
                    |> toPolar
                    |> Tuple.mapFirst (\mag -> mag * mag * -0.1)
                    |> fromPolar
                    |> vecScale (1 / p.mass)

            else
                ( 0, 0 )
    in
    vecAdd drag gravity


view : Model -> Html Msg
view model =
    svg [ SE.onClick Reset ]
        [ model.particles
            |> List.map viewParticle
            |> Svg.g []
        , rect ( screen.width, screen.height / 2 )
            [ translate ( 0, screen.height / 4 )
            , fill "#555"
            , style "opacity" "0.5"
            ]
        ]


particleRadius p =
    p.mass * 8


viewParticle p =
    circle (particleRadius p) [ fill "white", translate p.position ]
