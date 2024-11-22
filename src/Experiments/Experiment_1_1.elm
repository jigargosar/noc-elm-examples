module Experiments.Experiment_1_1 exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (style)
import Random exposing (Generator)
import Svg
import Svg.Attributes as SA
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { ticks : Int
    , particles : List Particle
    }


type alias Particle =
    { position : Vec
    , radius : Float
    , velocity : Vec
    }


type alias Vec =
    ( Float, Float )


screen =
    let
        ( w, h ) =
            ( 500, 500 )
    in
    { width = w, height = h, left = -w / 2, right = w / 2, top = -h / 2, bottom = h / 2 }


init : () -> ( Model, Cmd msg )
init () =
    let
        initialSeed =
            Random.initialSeed 0

        ( particles, seed ) =
            Random.step (Random.list 100 randomParticle) initialSeed
    in
    ( { ticks = 0, particles = particles }, Cmd.none )


type Msg
    = Tick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | ticks = model.ticks + 1, particles = List.map updateParticle model.particles }, Cmd.none )


updateParticle p =
    let
        position =
            vecAdd p.position p.velocity
                |> warpPosition
    in
    { p | position = position }


warpPosition ( x, y ) =
    ( if x > screen.right then
        screen.left

      else if x < screen.left then
        screen.right

      else
        x
    , if y > screen.bottom then
        screen.top

      else if y < screen.top then
        screen.bottom

      else
        y
    )


vecAdd =
    map2 (+)


map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


view : Model -> Html Msg
view model =
    Svg.svg
        [ SA.viewBox "-250 -250 500 500"
        , style "width" "500"
        , style "background" "#111"
        , style "fill" "none"
        , style "stroke" "none"
        ]
        ((points |> List.map viewPoint |> always [])
            ++ (model.particles |> List.map viewParticle)
        )


type alias Point =
    ( Float, Float )


randomParticle : Generator Particle
randomParticle =
    Random.map3 Particle
        randomPositionOnScreen
        (Random.float 10 15)
        randomVelocity


randomPositionOnScreen =
    Random.pair (Random.float screen.left screen.right) (Random.float screen.top screen.bottom)


randomVelocity =
    let
        randomMag =
            Random.float 0 0.05
    in
    Random.pair randomMag (Random.float 0 (2 * pi))
        |> Random.map fromPolar


randomPoint : Generator Point
randomPoint =
    -- Random.constant ( 100, 0 )
    let
        gen =
            -- Random.float -250 250
            Random.float -200 200
    in
    Random.pair gen gen


points =
    -- [ ( 100, 0 ) ]
    Random.step (Random.list 1000 randomPoint) (Random.initialSeed 1)
        |> Tuple.first


viewPoint ( x, y ) =
    Svg.circle
        [ SA.r "10"
        , translate ( x, y )
        , style "fill" "#FFF"
        , style "opacity" "0.1"
        ]
        []


viewParticle p =
    let
        ( x, y ) =
            p.position
    in
    Svg.circle
        [ SA.r (String.fromFloat p.radius)
        , translate ( x, y )
        , style "fill" "#bbb"
        , style "opacity" "0.5"
        ]
        []


px f =
    String.fromFloat f ++ "px"


translate ( x, y ) =
    style "translate" (px x ++ " " ++ px y)
