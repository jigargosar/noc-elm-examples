module Ch02.Eg09 exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as JD
import List.Extra
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


type alias Mouse =
    { position : Vec
    , down : Bool
    , click : Bool
    }


mouseClick : Bool -> Mouse -> Mouse
mouseClick bool mouse =
    { mouse | click = bool }


mouseDown : Bool -> Mouse -> Mouse
mouseDown bool mouse =
    { mouse | down = bool }


mouseMove : Vec -> Mouse -> Mouse
mouseMove position mouse =
    { mouse | position = position }


type alias Model =
    { particles : List Particle
    , mouse : Mouse
    }


type alias Particle =
    { position : Vec
    , velocity : Vec
    , mass : Float
    }


screen =
    Utils.screen


randomParticles =
    let
        randomPosition =
            Random.pair (Random.float screen.left screen.right)
                (Random.float screen.top screen.bottom)

        randomMass =
            Random.float 0.1 2

        initParticle position mass =
            { position = position, velocity = ( 0, 0 ), mass = mass }

        randomParticle =
            Random.map2 initParticle
                randomPosition
                randomMass
    in
    Random.list 10 randomParticle


init : () -> ( Model, Cmd msg )
init () =
    let
        ( particles, seed ) =
            Random.step randomParticles (Random.initialSeed 0)
    in
    ( { particles = particles
      , mouse = Mouse ( 0, 0 ) False False
      }
    , Cmd.none
    )


type Msg
    = Tick
    | MouseButton Bool
    | MouseClick
    | MouseMove String Float Float


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model
                | mouse = mouseClick False model.mouse
                , particles =
                    List.Extra.select model.particles
                        |> List.map (\( particle, attractors ) -> updateParticle attractors particle)
              }
            , Cmd.none
            )

        MouseButton isDown ->
            ( { model | mouse = mouseDown isDown model.mouse }, Cmd.none )

        MouseClick ->
            ( { model | mouse = mouseClick True model.mouse }, Cmd.none )

        MouseMove _ x y ->
            ( { model | mouse = mouseMove ( x + screen.left, y + screen.top ) model.mouse }, Cmd.none )


updateParticle : List Particle -> Particle -> Particle
updateParticle attractors p =
    let
        cumulativeAcceleration =
            List.foldl
                (\attractor acc ->
                    particleAcceleration attractor p
                        |> vecAdd acc
                )
                ( 0, 0 )
                attractors

        velocity =
            vecAdd p.velocity cumulativeAcceleration
    in
    { p
        | velocity = velocity
        , position = vecAdd p.position velocity
    }


attractorForceOnParticle : Particle -> Particle -> Vec
attractorForceOnParticle particle attractor =
    let
        force =
            vecSub attractor.position particle.position

        distance =
            force
                |> toPolar
                |> Tuple.first
                |> clamp 5 25

        gravitationalConstant =
            1

        strength =
            (gravitationalConstant * attractor.mass * particle.mass)
                / (distance * distance)
    in
    force
        |> vecSetMag strength


particleAcceleration : Particle -> Particle -> Vec
particleAcceleration a p =
    attractorForceOnParticle p a
        |> vecDiv p.mass


view : Model -> Html Msg
view model =
    svg
        [ SE.onMouseDown (MouseButton True)
        , SE.onMouseUp (MouseButton False)
        , SE.onClick MouseClick
        , SE.on "mousemove"
            (JD.map3 MouseMove
                (JD.at [ "target", "id" ] JD.string)
                (JD.field "offsetX" JD.float)
                (JD.field "offsetY" JD.float)
            )
        , SA.id "svg"
        ]
        (model.particles |> List.map viewParticle)


particleRadius p =
    p.mass * 8


viewParticle p =
    circle (particleRadius p) [ fill "white", translate p.position ]
