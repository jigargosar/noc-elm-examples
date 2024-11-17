module Example_1_2 exposing (main)

import Browser
import Html
import Html.Attributes exposing (style)
import Svg
import Svg.Attributes as SA
import Time


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Vec =
    ( Float, Float )


type alias Model =
    { ticks : Int
    , position : Vec
    , velocity : Vec
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { ticks = 0
      , position = ( 0, 0 )
      , velocity = ( 2.5, 2 )
      }
    , Cmd.none
    )


type Msg
    = Tick


subscriptions _ =
    Time.every (1000 / 60) (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            let
                ( dx, dy ) =
                    model.velocity

                ( x, y ) =
                    newPosition

                newPosition =
                    vecAdd model.position model.velocity

                velocity =
                    ( if (x < -320 && dx < 0) || (x > 320 && dx > 0) then
                        -dx

                      else
                        dx
                    , if (y < -120 && dy < 0) || (y > 120 && dy > 0) then
                        -dy

                      else
                        dy
                    )
            in
            ( { model | ticks = model.ticks + 1, position = newPosition, velocity = velocity }, Cmd.none )


vecAdd =
    map2 (+)


map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


view : Model -> Html.Html msg
view model =
    Svg.svg
        [ SA.viewBox "-320 -120 640 240"
        , style "display" "block"
        , style "width" "640"
        , style "fill" "none"
        , style "stroke" "none"
        , style "background" "#000"
        , style "shape-rendering" "geometric-precision"

        --, style "shape-rendering" "optimizeSpeed"
        --, style "shape-rendering" "crispEdges"
        ]
        [ let
            ( x, y ) =
                model.position
          in
          Svg.circle
            [ SA.r "20"
            , SA.cx (String.fromFloat x)
            , SA.cy (String.fromFloat y)
            , style "fill" "#444"
            , style "stroke" "#fff"
            , SA.strokeWidth "6"
            ]
            []
        ]
