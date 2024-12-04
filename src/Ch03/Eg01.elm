module Ch03.Eg01 exposing (main)

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


type alias Model =
    { angle : Float }


screen =
    Utils.screen


init : () -> ( Model, Cmd msg )
init () =
    ( { angle = 0 }, Cmd.none )


type Msg
    = Tick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | angle = model.angle + 0.1 }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    svg
        []
        [ group [ rotate model.angle, strokeWidth 2 ]
            [ polyline [ ( -50, 0 ), ( 50, 0 ) ] [ stroke "white" ]
            , circle 8 [ fill "#444", stroke "#fff", translate ( -50, 0 ) ]
            , circle 8 [ fill "#444", stroke "#fff", translate ( 50, 0 ) ]
            ]
        ]
