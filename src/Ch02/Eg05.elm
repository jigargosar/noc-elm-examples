module Ch02.Eg05 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
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


initParticle total i =
    let
        x =
            (screen.width * (toFloat i + 0.5) / toFloat total) + screen.left
    in
    { position = ( x, 30 ), velocity = ( 0, 0 ), mass = toFloat 1 * 2 }


initParticles total =
    List.range 0 (total - 1) |> List.map (initParticle total)


init : () -> ( Model, Cmd msg )
init () =
    ( { particles = initParticles 10 }
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
