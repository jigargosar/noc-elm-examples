module Ch02.Eg05 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random
import Random.Extra
import Svg exposing (text)
import Svg.Attributes as SA
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
    { particles : List Particle }


type alias Particle =
    { position : Vec
    , velocity : Vec
    , mass : Float
    }


initParticle total i mass =
    let
        x =
            (screen.width * (toFloat i + 0.5) / toFloat total) + screen.left
    in
    { position = ( x, screen.top ), velocity = ( 0, 0 ), mass = mass }


randomParticle total i =
    Random.float 0.5 3
        |> Random.map (initParticle total i)


randomParticles : Int -> Random.Generator (List Particle)
randomParticles total =
    List.range 0 (total - 1)
        |> List.map (randomParticle total)
        |> Random.Extra.combine


init : () -> ( Model, Cmd msg )
init () =
    let
        ( particles, seed ) =
            Random.step (randomParticles 10) (Random.initialSeed 0)
    in
    ( { particles = particles }
    , Cmd.none
    )


type Msg
    = Tick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    svg []
        [ circle 20 [ fill "#fff" ]
        , model.particles
            |> List.map viewParticle
            |> Svg.g []
        ]


particleRadius p =
    p.mass * 8


viewParticle p =
    circle (particleRadius p) [ fill "white", translate p.position ]
