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
    in
    svg
        [ SA.viewBox "-250 -250 500 500"
        , style "width" "500"
        ]
        [ List.map viewTrackGP trackGPs
            |> Svg.g []
        ]


viewTrackGP gp =
    rect cellSize [ fill "dodgerblue", stroke "white", translate (gpToScreen gp) ]


gpToScreen ( x, y ) =
    let
        ( cw, ch ) =
            cellSize

        ( w, h ) =
            size
    in
    ( (toFloat x + 0.5) * cw - w / 2, (toFloat y + 0.5) * ch - h / 2 )


stroke =
    style "stroke"


rect ( w, h ) attrs =
    Svg.rect
        (SA.x (String.fromFloat (-w / 2))
            :: SA.y (String.fromFloat (-h / 2))
            :: SA.width (String.fromFloat w)
            :: SA.height (String.fromFloat h)
            :: attrs
        )
        []
