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


type alias Model =
    { ticks : Int
    , x : Float
    , y : Float
    , dx : Float
    , dy : Float
    }


init () =
    ( { ticks = 0
      , x = 0
      , y = 0
      , dx = 2.5
      , dy = 2
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
                    ( model.dx, model.dy )

                ( x, y ) =
                    ( model.x + dx, model.y + dy )

                ( ndx, ndy ) =
                    ( if ( x < -320 && dx < 0  )|| ( x > 320 && dx > 0  )then
                        -dx

                      else
                        dx
                    , if ( y < -120 && dy < 0 ) || ( y > 120 && dy > 0  )then
                        -dy

                      else
                        dy
                    )
            in
            ( { model | ticks = model.ticks + 1, x = x, y = y, dx = ndx, dy = ndy }, Cmd.none )


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
        [ Svg.circle
            [ SA.r "20"
            , SA.cx (String.fromFloat model.x)
            , SA.cy (String.fromFloat model.y)
            , style "fill" "#444"
            , style "stroke" "#fff"
            , SA.strokeWidth "6"
            ]
            []
        ]
