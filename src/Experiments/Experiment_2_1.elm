module Experiments.Experiment_2_1 exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Random exposing (Generator, Seed)
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
    , seed : Seed
    }


type alias Particle =
    { position : Vec
    , radius : Float
    , velocity : Vec
    }


type alias Screen =
    { width : Float
    , height : Float
    , left : Float
    , right : Float
    , top : Float
    , bottom : Float
    }


screen : Screen
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

        randomParticles =
            Random.list 1000 randomParticle

        ( particles, seed ) =
            Random.step randomParticles initialSeed
    in
    ( { ticks = 0, particles = particles, seed = seed }, Cmd.none )


type Msg
    = Tick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model
                | ticks = model.ticks + 1
                , particles = List.map updateParticle model.particles
              }
            , Cmd.none
            )


updateParticle p =
    let
        position =
            vecAdd p.position p.velocity
                --|> warpPosition
                |> warpPositionInScreen (growScreenByRadius p.radius screen)
    in
    { p | position = position }


growScreenByRadius r s =
    let
        d =
            r * 2

        ( w, h ) =
            ( s.width + d, s.height + d )
    in
    { width = w, height = h, left = -w / 2, right = w / 2, top = -h / 2, bottom = h / 2 }


warpPositionInScreen s ( x, y ) =
    ( if x > s.right then
        s.left

      else if x < s.left then
        s.right

      else
        x
    , if y > s.bottom then
        s.top

      else if y < s.top then
        s.bottom

      else
        y
    )


view : Model -> Html Msg
view model =
    Svg.svg
        [ SA.viewBox "-250 -250 500 500"
        , SA.viewBox "0 0 500 500"
        , SA.viewBox "-250 -250 500 500"
        , style "width" "500"
        , style "background" "#111"
        , style "fill" "none"
        , style "stroke" "none"
        ]
        --(model.particles |> List.map viewParticle)
        [ Svg.defs []
            [ Svg.pattern
                [ SA.id "pat"
                , SA.x "-25"
                , SA.y "-25"
                , SA.width "50"
                , SA.height "50"
                , SA.patternUnits "userSpaceOnUse"
                , SA.patternTransform "translate(0, 0) "
                ]
                [ Svg.g [ SA.transform "translate(25, 25)" ]
                    [ Svg.circle [ SA.cx "0", SA.r "10", style "fill" "red" ] []
                    , Svg.rect [ SA.x "-20", SA.y "-20", SA.width "40", SA.height "40", SA.stroke "white" ] []
                    ]
                ]
            ]
        , Svg.rect [ SA.x "-100", SA.y "-100", SA.width "200", SA.height "200", SA.fill "url(#pat)", SA.stroke "white" ] []
        , Svg.circle [ SA.r "10", SA.fill "dodgerblue" ] []
        ]


type alias Point =
    ( Float, Float )


randomParticle : Generator Particle
randomParticle =
    randomRadius
        |> Random.andThen
            (\radius ->
                Random.map2 (\position velocity -> Particle position radius velocity)
                    randomPositionOnScreen
                    (randomVelocityFromRadius radius)
            )


randomVelocityFromRadius r =
    let
        randomMag =
            Random.float 0 1
                |> always (Random.constant ((r * r) * 0.005))
    in
    Random.pair randomMag (Random.float 0 (2 * pi))
        |> Random.map fromPolar


randomRadius =
    Random.float 5 15


randomPositionOnScreen =
    Random.pair (Random.float screen.left screen.right) (Random.float screen.top screen.bottom)


randomVelocity =
    let
        randomMag =
            Random.float 0 1
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


viewParticle p =
    let
        ( x, y ) =
            p.position
    in
    Svg.circle
        [ SA.r (String.fromFloat p.radius)
        , translate ( x, y )
        , style "fill" "#fff"
        , style "opacity" "0.1"
        ]
        []


type alias Vec =
    ( Float, Float )


vecAdd =
    map2 (+)


map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


px f =
    String.fromFloat f ++ "px"


translate ( x, y ) =
    style "translate" (px x ++ " " ++ px y)
