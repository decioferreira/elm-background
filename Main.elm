module Main exposing (..)

import AnimationFrame
import Collage exposing (Form)
import Color
import Element
import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Mouse
import Random exposing (Seed)
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
    { columns : Int
    , rows : Int
    }


type alias Point =
    { x : Float
    , xOrigin : Float
    , xStart : Float
    , xEnd : Float
    , y : Float
    , yOrigin : Float
    , yStart : Float
    , yEnd : Float
    , duration : Time
    , elapsed : Time
    , radius : Float
    }


type alias Model =
    { configuration : Configuration
    , mousePosition : Mouse.Position
    , points : List Point
    , windowSize : Window.Size
    , seed : Seed
    }



-- UPDATE


type Msg
    = Tick Time
    | MouseMove Mouse.Position
    | WindowResize Window.Size
    | NoOp


positionModelPoint : Configuration -> Window.Size -> Point -> ( Int, Int ) -> Point
positionModelPoint { columns, rows } { width, height } point ( column, row ) =
    let
        halfWidth =
            (toFloat width) / 2

        halfHeight =
            (toFloat height) / 2

        columnDistance =
            (toFloat width) / (toFloat columns)

        rowDistance =
            (toFloat height) / (toFloat rows)
    in
        { point
            | xOrigin = -halfWidth + ((toFloat column) * columnDistance)
            , yOrigin = -halfHeight + ((toFloat row) * rowDistance)
        }


positionModelPoints : Configuration -> Window.Size -> List Point -> List Point
positionModelPoints configuration windowSize points =
    let
        positions =
            List.concatMap (\x -> List.map (\y -> ( x, y )) [0..configuration.rows])
                [0..configuration.columns]
    in
        List.map2 (positionModelPoint configuration windowSize) points positions


easing : Time -> Float -> Float -> Time -> Float
easing t b c d =
    (((c * t) / d) + b)


animatePoint : Time -> Point -> ( List Point, Seed ) -> ( List Point, Seed )
animatePoint elapsed point ( points, seed ) =
    if point.elapsed > point.duration then
        let
            ( point', seed' ) =
                resetRandomEndPosition ( { point | elapsed = 0, xStart = point.xEnd, yStart = point.yEnd }, seed )
                    |> resetRandomDuration
        in
            ( points ++ [ point' ], seed' )
    else
        let
            elapsed' =
                point.elapsed + elapsed

            x =
                easing elapsed' point.xStart (point.xEnd - point.xStart) point.duration

            y =
                easing elapsed' point.yStart (point.yEnd - point.yStart) point.duration
        in
            ( points ++ [ { point | elapsed = elapsed', x = x, y = y } ], seed )


animate : Time -> Model -> Model
animate elapsed model =
    let
        ( points, seed ) =
            List.foldr (animatePoint elapsed) ( [], model.seed ) model.points
    in
        { model | points = points, seed = seed }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                model' =
                    model
                        |> animate dt
            in
                ( model', Cmd.none )

        MouseMove position ->
            ( { model | mousePosition = position }, Cmd.none )

        WindowResize newWindowSize ->
            let
                points' =
                    positionModelPoints model.configuration newWindowSize model.points
            in
                ( { model | points = points', windowSize = newWindowSize }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


distanceBetweenTwoPoints : ( Float, Float ) -> ( Float, Float ) -> Float
distanceBetweenTwoPoints ( x1, y1 ) ( x2, y2 ) =
    ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)


displayPoint : Window.Size -> Mouse.Position -> Point -> Form
displayPoint windowSize mousePosition { x, xOrigin, y, yOrigin, radius } =
    let
        xMouse =
            (toFloat mousePosition.x) - ((toFloat windowSize.width) / 2)

        yMouse =
            -(toFloat mousePosition.y) + ((toFloat windowSize.height) / 2)

        distance =
            distanceBetweenTwoPoints ( xMouse, yMouse ) ( xOrigin, yOrigin )
                |> abs

        active =
            if distance < 4000 then
                { lines = 0.3, circle = 0.6 }
            else if distance < 20000 then
                { lines = 0.1, circle = 0.3 }
            else if distance < 40000 then
                { lines = 0.02, circle = 0.1 }
            else
                { lines = 0, circle = 0 }
    in
        Collage.filled (Color.rgba 0 0 0 active.circle) (Collage.circle radius)
            |> Collage.move ( xOrigin, yOrigin )
            |> Collage.move ( x, y )


view : Model -> Html Msg
view { mousePosition, points, windowSize } =
    body []
        [ List.map (displayPoint windowSize mousePosition) points
            |> Collage.collage windowSize.width windowSize.height
            |> Element.container windowSize.width windowSize.height Element.middle
            |> Element.toHtml
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Mouse.moves MouseMove
        , Window.resizes WindowResize
        ]



-- INIT


addRandomData : Point -> ( List Point, Seed ) -> ( List Point, Seed )
addRandomData point ( points, seed ) =
    let
        ( point', seed' ) =
            addRandomRadius ( point, seed )
                |> resetRandomEndPosition
                |> resetRandomDuration
    in
        ( points ++ [ point' ], seed' )


addRandomRadius : ( Point, Seed ) -> ( Point, Seed )
addRandomRadius ( point, seed ) =
    let
        ( randomRadius, seed' ) =
            Random.step (Random.float 0 2) seed
    in
        ( { point | radius = point.radius + randomRadius }, seed' )


resetRandomEndPosition : ( Point, Seed ) -> ( Point, Seed )
resetRandomEndPosition ( point, seed ) =
    let
        randomRange =
            (Random.float -50 50)

        ( ( xEnd, yEnd ), seed' ) =
            Random.step (Random.pair randomRange randomRange) seed
    in
        ( { point | xEnd = xEnd, yEnd = yEnd }, seed' )


resetRandomDuration : ( Point, Seed ) -> ( Point, Seed )
resetRandomDuration ( point, seed ) =
    let
        ( duration, seed' ) =
            Random.step (Random.float 1000 2000) seed
    in
        ( { point | duration = duration }, seed' )


init : ( Model, Cmd Msg )
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
            , elapsed = 0
            , radius = 2
            }

        configuration =
            { columns = 20
            , rows = 20
            }

        points =
            List.repeat ((configuration.columns + 1) * (configuration.rows + 1)) defaultPoint
                |> List.foldr addRandomData ( [], Random.initialSeed 31415 )
                |> fst

        model =
            { configuration = configuration
            , mousePosition = { x = 0, y = 0 }
            , points = points
            , windowSize = { width = 0, height = 0 }
            , seed = Random.initialSeed 31415
            }
    in
        ( model
        , Cmd.batch
            [ Task.perform (always NoOp) WindowResize Window.size
            ]
        )
