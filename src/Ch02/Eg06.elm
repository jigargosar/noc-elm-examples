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
    ( { particles = [ { position = ( 50, 50 ), velocity = ( 0, 0 ), mass = 2 } ]
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
                | particles = List.map updateParticle model.particles
                , mouse = mouseClick False model.mouse
              }
            , Cmd.none
            )

        MouseButton isDown ->
            ( { model | mouse = mouseDown isDown model.mouse }, Cmd.none )

        MouseClick ->
            ( { model | mouse = mouseClick True model.mouse }, Cmd.none )

        MouseMove _ x y ->
            ( { model | mouse = mouseMove ( x + screen.left, y + screen.top ) model.mouse }, Cmd.none )


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


view model =
    div []
        [ viewSvg model
        , div [] [ text (Debug.toString model.mouse) ]
        ]


viewSvg : Model -> Html Msg
viewSvg model =
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
            |> always (text "")
        , circle (attractorRadius attractor)
            [ SA.id "circle"
            , fill "#555"
            , stroke "#fff"
            , SA.strokeWidth "4"
            ]
        ]


attractorRadius a =
    a.mass


particleRadius p =
    p.mass * 8


viewParticle p =
    circle (particleRadius p) [ fill "white", translate p.position ]
