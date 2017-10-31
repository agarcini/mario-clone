module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import AnimationFrame
import Keyboard exposing (KeyCode)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


spriteWidth : Float
spriteWidth =
    16


acceleration : Float
acceleration =
    0.75


init : ( Model, Cmd Msg )
init =
    ( { locationX = spriteWidth / 2, locationY = 0, facing = Right 0, jumping = False, animating = False, tick = 0 }, Cmd.none )


type Msg
    = Tick Float
    | KeyDown KeyCode
    | KeyUp KeyCode


type alias Model =
    { locationX : Float
    , locationY : Float
    , facing : Facing
    , jumping : Bool
    , animating : Bool
    , tick : Float
    }


type Facing
    = Left Float
    | Right Float


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown 39 ->
            ( { model
                | animating = True
                , facing =
                    case model.facing of
                        Right 0 ->
                            Right 10

                        Right velocityX ->
                            Right velocityX

                        _ ->
                            Right 10
              }
            , Cmd.none
            )

        KeyUp 39 ->
            ( { model | animating = False, facing = Right 0 }, Cmd.none )

        KeyDown 37 ->
            ( { model
                | animating = True
                , facing =
                    case model.facing of
                        Left 0 ->
                            Left 10

                        Left velocityX ->
                            Left velocityX

                        _ ->
                            Left 10
              }
            , Cmd.none
            )

        KeyUp 37 ->
            ( { model | animating = False, facing = Left 0 }, Cmd.none )

        KeyDown 32 ->
            ( { model | animating = False, jumping = True }, Cmd.none )

        KeyDown _ ->
            ( model, Cmd.none )

        KeyUp _ ->
            ( model, Cmd.none )

        Tick delta ->
            tick delta model


tick : Float -> Model -> ( Model, Cmd Msg )
tick delta model =
    ( model
        |> updateTick delta
        |> updateFacing delta
        |> updateLocation delta
    , Cmd.none
    )


updateTick : Float -> Model -> Model
updateTick delta model =
    { model
        | tick =
            if model.animating == False then
                0
            else if model.tick > 144 then
                -- 144 is how many ticks for full animation cycle
                0
            else
                model.tick + delta
    }


updateFacing : Float -> Model -> Model
updateFacing delta model =
    { model
        | facing =
            case model.facing of
                Right 0 ->
                    Right 0

                Right velocityX ->
                    if velocityX + acceleration >= 300 then
                        Right 300
                    else
                        Right (velocityX + (acceleration * delta))

                Left 0 ->
                    Left 0

                Left velocityX ->
                    if velocityX + acceleration >= 300 then
                        Left 300
                    else
                        Left (velocityX + (acceleration * delta))
    }


updateLocation : Float -> Model -> Model
updateLocation delta model =
    { model
        | locationX =
            case model.facing of
                Right 0 ->
                    model.locationX

                Right velocityX ->
                    model.locationX + ((velocityX / 1000) * delta)

                Left 0 ->
                    model.locationX

                Left velocityX ->
                    model.locationX - ((velocityX / 1000) * delta)
    }


view : Model -> Html Msg
view model =
    let
        backgroundPosition =
            if model.animating == False then
                "-10px"
            else if model.tick <= 48 then
                "-45px"
            else if model.tick <= 96 then
                "-80px"
            else
                "-10px"

        scale =
            case model.facing of
                Right _ ->
                    "scale(2)"

                Left _ ->
                    "scale(-2, 2)"
    in
        div
            [ style
                [ ( "background-image", "url(/img/mario.png)" )
                , ( "display", "table" )
                , ( "width", toString spriteWidth ++ "px" )
                , ( "height", "27px" )
                , ( "position", "absolute" )
                , ( "left", toString model.locationX ++ "px" )
                , ( "bottom", toString model.locationY ++ "px" )
                , ( "border", "1px solid black" )
                , ( "background-repeat", "no-repeat" )
                , ( "background-position", backgroundPosition )
                , ( "transform", scale )
                , ( "transform-origin", "center bottom" )
                ]
            ]
            []
