import AnimationFrame
import Collage exposing (Form)
import Color
import Element
import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Random
import Task
import Time exposing (Time)
import Window

main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Configuration =
  { columns: Int
  , rows: Int
  }

type alias Point =
  { x: Float
  , xOrigin: Float
  , xStart: Float
  , xEnd: Float
  , y: Float
  , yOrigin: Float
  , yStart: Float
  , yEnd: Float
  , duration: Time
  }

type alias Model =
  { configuration : Configuration
  , points : List Point
  , windowSize : Window.Size
  }


-- UPDATE

type Msg
  = Tick Time
  | WindowResize Window.Size
  | NoOp


positionModelPoint : Configuration -> Window.Size -> Point -> (Int, Int) -> Point
positionModelPoint { columns, rows } { width, height } point (column, row) =
  let
    halfWidth = (toFloat width) / 2
    halfHeight = (toFloat height) / 2
    columnDistance = (toFloat width) / (toFloat columns)
    rowDistance = (toFloat height) / (toFloat rows)
  in
    { point
    | xOrigin = -halfWidth + ((toFloat column) * columnDistance)
    , yOrigin = -halfHeight + ((toFloat row) * rowDistance)
    }

positionModelPoints : Configuration -> Window.Size -> List Point -> List Point
positionModelPoints configuration windowSize points =
  let
    positions = List.concatMap
      (\x -> List.map (\y -> (x, y)) [0..configuration.rows])
      [0..configuration.columns]
  in
    List.map2 (positionModelPoint configuration windowSize) points positions


animatePoint : Time -> Point -> Point
animatePoint elapsed point =
  point


animate : Time -> Model -> Model
animate elapsed model =
  let
    points = model.points
      |> List.map (animatePoint elapsed)
  in
    { model | points = points }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick dt ->
      let
        model' = model
          |> animate dt
      in
        (model', Cmd.none)

    WindowResize newWindowSize ->
      let
        points' = positionModelPoints model.configuration newWindowSize model.points
        log = Debug.log "points'" points'
      in
        ( { model | points = points', windowSize = newWindowSize }, Cmd.none )

    NoOp ->
      (model, Cmd.none)

-- VIEW

displayPoint : Point -> Form
displayPoint { x, xOrigin, y, yOrigin } =
  let
    randomRadius = 2 -- + (Random.float 0 1)
  in
    Collage.filled (Color.rgba 0 0 0 0.3) (Collage.circle randomRadius)
      |> Collage.move (xOrigin, yOrigin)
      |> Collage.move (x, y)


view : Model -> Html Msg
view { points, windowSize } =
  body []
    [
      List.map displayPoint points
        |> Collage.collage windowSize.width windowSize.height
        |> Element.container windowSize.width windowSize.height Element.middle
        |> Element.toHtml
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs Tick
    , Window.resizes WindowResize
    ]


-- INIT

init : (Model, Cmd Msg)
init =
  let
    defaultPoint =
      { x = 0
      , xOrigin = 0
      , xStart = 0
      , xEnd = 0
      , y = 0
      , yOrigin = 0
      , yStart = 0
      , yEnd = 0
      , duration = 0
      }
    configuration =
      { columns = 20
      , rows = 20
      }
    model =
      { configuration = configuration
      , points = List.repeat ((configuration.columns + 1) * (configuration.rows + 1)) defaultPoint
      , windowSize = { width = 0, height = 0 }
      }
  in
    ( model
    , Cmd.batch
        [ Task.perform (always NoOp) WindowResize Window.size
        ]
    )
