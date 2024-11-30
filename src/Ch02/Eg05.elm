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


initParticle i =
    let
        x =
            --(screen.width / 10) * (toFloat i + 0.5) + screen.left
            (screen.width * (toFloat i + 0.5) / 10) + screen.left
    in
    Particle ( x, 30 ) ( 0, 0 ) (toFloat 1 * 2)


init : () -> ( Model, Cmd msg )
init () =
    ( { particles = List.range 0 9 |> List.map initParticle }
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


viewParticle p =
    circle (p.mass * 8) [ fill "white", translate p.position ]
