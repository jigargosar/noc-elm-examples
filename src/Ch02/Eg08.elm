module Ch02.Eg08 exposing (main)

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
    { particleA : Particle
    , particleB : Particle
    , mouse : Mouse
    }


type alias Particle =
    { position : Vec
    , velocity : Vec
    , mass : Float
    }


screen =
    Utils.screen


init : () -> ( Model, Cmd msg )
init () =
    ( { particleA = Particle ( 320, 40 ) ( 1, 0 ) 8
      , particleB = Particle ( 320, 200 ) ( -1, 0 ) 8
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
              }
            , Cmd.none
            )

        MouseButton isDown ->
            ( { model | mouse = mouseDown isDown model.mouse }, Cmd.none )

        MouseClick ->
            ( { model | mouse = mouseClick True model.mouse }, Cmd.none )

        MouseMove _ x y ->
            ( { model | mouse = mouseMove ( x + screen.left, y + screen.top ) model.mouse }, Cmd.none )


updateParticle a p =
    let
        velocity =
            vecAdd p.velocity (particleAcceleration a p)
    in
    { p
        | velocity = velocity
        , position = vecAdd p.position velocity
    }


particleAcceleration _ _ =
    ( 0, 0 )



--attractorForceOnParticle : Particle -> Attractor -> Vec
--attractorForceOnParticle particle attractor =
--    let
--        force =
--            vecSub attractor.position particle.position
--
--        distance =
--            force
--                |> toPolar
--                |> Tuple.first
--                |> clamp 5 25
--
--        gravitationalConstant =
--            1
--
--        strength =
--            (gravitationalConstant * attractor.mass * particle.mass)
--                / (distance * distance)
--    in
--    force
--        |> vecSetMag strength
--
--
--particleAcceleration : Attractor -> Particle -> Vec
--particleAcceleration a p =
--    attractorForceOnParticle p a
--        |> vecDiv p.mass
--


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
        [ viewParticle model.particleA
        , viewParticle model.particleB
        ]


particleRadius p =
    sqrt p.mass * 2


viewParticle p =
    circle (particleRadius p) [ fill "white", translate p.position ]
