module Ch02.Eg06 exposing (..)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as JD
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
    , attractor : Attractor
    , mouse : Mouse
    }


type alias Attractor =
    { position : Vec
    , mass : Float
    }


type alias Particle =
    { position : Vec
    , velocity : Vec
    , mass : Float
    }


init : () -> ( Model, Cmd msg )
init () =
    ( { particles = [ { position = ( 50, -50 ), velocity = ( 2, 2 ), mass = 2 } ]
      , attractor = { position = ( 0, 0 ), mass = 20 }
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
                | particles = List.map (updateParticle model.attractor) model.particles
                , mouse = mouseClick False model.mouse
                , attractor = updateAttractor model.mouse model.attractor
              }
            , Cmd.none
            )

        MouseButton isDown ->
            ( { model | mouse = mouseDown isDown model.mouse }, Cmd.none )

        MouseClick ->
            ( { model | mouse = mouseClick True model.mouse }, Cmd.none )

        MouseMove _ x y ->
            ( { model | mouse = mouseMove ( x + screen.left, y + screen.top ) model.mouse }, Cmd.none )


updateAttractor mouse attractor =
    if mouse.down then
        { attractor | position = mouse.position }

    else
        attractor


updateParticle a p =
    let
        velocity =
            vecAdd p.velocity (particleAcceleration a p)
    in
    { p
        | velocity = velocity
        , position = vecAdd p.position velocity
    }


attractorForceOnParticle : Particle -> Attractor -> Vec
attractorForceOnParticle particle attractor =
    let
        force =
            vecSub attractor.position particle.position

        distance =
            force
                |> toPolar
                |> Tuple.first
                |> clamp 5 15

        gravitationalConstant =
            1

        strength =
            (gravitationalConstant * attractor.mass * particle.mass)
                / (distance * distance)
    in
    force
        |> vecSetMag strength


particleAcceleration : Attractor -> Particle -> Vec
particleAcceleration a p =
    attractorForceOnParticle p a
        |> vecDiv p.mass


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


view : Model -> Html Msg
view model =
    let
        attractor =
            model.attractor
    in
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
        [ model.particles
            |> List.map viewParticle
            |> Svg.g []
        , circle (attractorRadius attractor)
            [ SA.id "circle"
            , fill "#555"
            , stroke "#fff"
            , SA.strokeWidth "4"
            , translate attractor.position
            ]
        ]


attractorRadius a =
    a.mass


particleRadius p =
    p.mass * 8


viewParticle p =
    circle (particleRadius p) [ fill "white", translate p.position ]
