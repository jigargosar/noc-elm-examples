module Experiments.Ex03_01 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import List.Extra
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


type alias GP =
    ( Int, Int )


type alias TrackSeg =
    { gp : GP
    , resource : Bool
    }


type alias Model =
    { track : List TrackSeg
    }


init : () -> ( Model, Cmd msg )
init () =
    let
        track =
            List.range 1 18
                |> List.indexedMap
                    (\i x ->
                        TrackSeg ( x, 5 )
                            (if List.member i [ 1, 3, 6 ] then
                                True

                             else
                                False
                            )
                    )
    in
    ( { track = track
      }
    , Cmd.none
    )


removeResource ts =
    { ts | resource = False }


addResource ts =
    { ts | resource = True }


type Msg
    = Tick


subscriptions : Model -> Sub Msg
subscriptions _ =
    --Time.every (1000 / 60) (always Tick)
    Time.every 400 (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            let
                track : Track
                track =
                    List.range 0 (List.length model.track - 1)
                        |> List.foldr trackCheckAtIndex model.track
            in
            ( { model | track = track }, Cmd.none )


type alias Track =
    List TrackSeg


trackCheckAtIndex : Int -> Track -> Track
trackCheckAtIndex i list =
    case ( List.Extra.getAt (i - 1) list, List.Extra.getAt i list ) of
        ( Just prev, Just curr ) ->
            if not curr.resource && prev.resource then
                list
                    |> List.Extra.updateAt i addResource
                    |> List.Extra.updateAt (i - 1) removeResource

            else
                list

        _ ->
            list


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
        ( width, height ) =
            size
    in
    svg
        [ viewBoxFromSize size
        , widthInPx width
        , heightInPx height
        ]
        [ List.map viewTrackSegment model.track
            |> group []
        ]


viewTrackSegment ts =
    [ viewTrackGP ts.gp
    , if ts.resource then
        viewResourceGP ts.gp

      else
        group [] []
    ]
        |> group []


viewResourceGP gp =
    rect (vecScale 0.6 cellSize) [ fill "pink", stroke "white", translateToGP gp ]


viewTrackGP gp =
    rect cellSize [ fill "dodgerblue", stroke "white", translateToGP gp ]


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
