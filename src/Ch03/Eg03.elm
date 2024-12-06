module Ch03.Eg03 exposing (main)

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
    { particle : Particle
    , mouse : Mouse
    }


type alias Particle =
    { position : Vec
    , velocity : Vec
    }


screen =
    Utils.screen


init : () -> ( Model, Cmd msg )
init () =
    ( { particle = Particle ( 0, 0 ) ( 0, 0 )
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
                | particle =
                    updateParticle model.mouse model.particle
                        |> wrapAroundEdges
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


wrapAroundEdges p =
    let
        ( x, y ) =
            p.position

        position =
            ( if x < screen.left then
                screen.right

              else if x > screen.right then
                screen.left

              else
                x
            , if y < screen.top then
                screen.bottom

              else if y > screen.bottom then
                screen.top

              else
                y
            )
    in
    { p | position = position }


updateParticle mouse p =
    let
        acceleration =
            vecSub mouse.position p.position
                |> vecNormalize
                |> vecScale 0.5

        velocity =
            vecAdd p.velocity acceleration
                |> mapMag (atMost 4)
    in
    { p
        | velocity = velocity
        , position = vecAdd p.position velocity
    }


mapMag fn v =
    v
        |> toPolar
        |> Tuple.mapFirst fn
        |> fromPolar


vecNormalize ( x, y ) =
    ( 1, atan2 y x )
        |> fromPolar


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
        [ viewParticle model.particle
        ]


viewParticle p =
    let
        angle =
            p.velocity |> toPolar |> Tuple.second
    in
    group
        [ translate p.position
        , rotate angle
        , fill "#444"
        , stroke "white"
        , strokeWidth 1
        ]
        [ rect ( 30, 10 ) []
        ]
