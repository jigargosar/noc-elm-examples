module Experiments.Ex03_01 exposing (..)

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
    {}


init : () -> ( Model, Cmd msg )
init () =
    ( Model
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


size =
    ( 500, 500 )


gridSize =
    ( 20, 20 )


cellSize =
    let
        ( w, h ) =
            size

        ( gw, gh ) =
            gridSize
    in
    ( w / gw, h / gh )


view : Model -> Html Msg
view model =
    let
        trackGPs =
            List.range 1 18
                |> List.map (\x -> ( x, 5 ))

        resourceGPs =
            List.take 1 trackGPs

        ( width, height ) =
            size
    in
    svg
        [ viewBoxFromSize size
        , widthInPx width
        , heightInPx height
        ]
        [ List.map viewResourceGP trackGPs
            |> group []
        , List.map viewTrackGP trackGPs
            |> group []
        , List.map viewResourceGP resourceGPs
            |> group []
        ]


viewResourceGP gp =
    rect (vecScale 0.6 cellSize) [ fill "pink", stroke "gold", translateToGP gp ]


viewTrackGP gp =
    rect cellSize [ fill "dodgerblue", stroke "gold", translateToGP gp ]


translateToGP gp =
    translate (gpToScreen gp)


gpToScreen ( x, y ) =
    let
        ( cw, ch ) =
            cellSize

        ( w, h ) =
            size
    in
    ( (toFloat x + 0.5) * cw - w / 2, (toFloat y + 0.5) * ch - h / 2 )
