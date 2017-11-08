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


gravity : Float
gravity =
    1.25


maxVelocityX : Float
maxVelocityX =
    300


jumpVelocity : Float
jumpVelocity =
    600


terminalVelocity : Float
terminalVelocity =
    800


idleFrame : String
idleFrame =
    "-10px"


jumpingFrame : String
jumpingFrame =
    "-80px"


runningFrame1 : String
runningFrame1 =
    "-10px"


runningFrame2 : String
runningFrame2 =
    "-45px"


runningFrame3 : String
runningFrame3 =
    "-80px"


init : ( Model, Cmd Msg )
init =
    ( { locationX = spriteWidth / 2, locationY = 0, facing = Right 0, jumping = Grounded, tick = 0 }, Cmd.none )


type Msg
    = Tick Float
    | KeyDown KeyCode
    | KeyUp KeyCode


type alias Model =
    { locationX : Float
    , locationY : Float
    , facing : Facing
    , jumping : Jumping
    , tick : Float
    }


type Facing
    = Left Float
    | Right Float


type Jumping
    = Grounded
    | Jumping Float
    | Falling Float


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
                | facing =
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
            ( { model | facing = Right 0 }, Cmd.none )

        KeyDown 37 ->
            ( { model
                | facing =
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
            ( { model | facing = Left 0 }, Cmd.none )

        KeyDown 32 ->
            ( { model | jumping = Jumping jumpVelocity }, Cmd.none )

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
        |> updateVelocity delta
        |> updateLocation delta
    , Cmd.none
    )


updateTick : Float -> Model -> Model
updateTick delta model =
    { model
        | tick =
            if not (isRunning model) then
                0
            else if model.tick > 144 then
                -- 144 is how many ticks for full animation cycle
                0
            else
                model.tick + delta
    }


updateVelocity : Float -> Model -> Model
updateVelocity delta model =
    { model
        | facing =
            case model.facing of
                Right 0 ->
                    Right 0

                Right velocityX ->
                    if velocityX + (acceleration * delta) >= maxVelocityX then
                        Right maxVelocityX
                    else
                        Right (velocityX + (acceleration * delta))

                Left 0 ->
                    Left 0

                Left velocityX ->
                    if velocityX + (acceleration * delta) >= maxVelocityX then
                        Left maxVelocityX
                    else
                        Left (velocityX + (acceleration * delta))
        , jumping =
            case model.jumping of
                Grounded ->
                    Grounded

                Jumping velocityY ->
                    if velocityY - (gravity * delta) > 0 then
                        Jumping (velocityY - (gravity * delta))
                    else
                        Falling 0

                Falling velocityY ->
                    if isGrounded model then
                        Grounded
                    else if velocityY + (gravity * delta) <= terminalVelocity then
                        Falling (velocityY + (gravity * delta))
                    else
                        Falling terminalVelocity
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
        , locationY =
            case model.jumping of
                Grounded ->
                    0

                Jumping velocityY ->
                    model.locationY + ((velocityY / 1000) * delta)

                Falling velocityY ->
                    model.locationY - ((velocityY / 1000) * delta)
    }


isRunning : Model -> Bool
isRunning model =
    case model.facing of
        Right velocityX ->
            not (isAirborne model.jumping) && velocityX > 0

        Left velocityX ->
            not (isAirborne model.jumping) && velocityX > 0


isGrounded : Model -> Bool
isGrounded model =
    if model.locationY <= 0 then
        True
    else
        False


isAirborne : Jumping -> Bool
isAirborne jumping =
    case jumping of
        Jumping _ ->
            True

        Falling _ ->
            True

        Grounded ->
            False


animationFrame : Model -> String
animationFrame model =
    if isAirborne model.jumping then
        jumpingFrame
    else if not (isRunning model) then
        idleFrame
    else if model.tick <= 48 then
        runningFrame2
    else if model.tick <= 96 then
        runningFrame3
    else
        runningFrame1


spriteFacing : Model -> String
spriteFacing model =
    case model.facing of
        Right _ ->
            "scale(2)"

        Left _ ->
            "scale(-2, 2)"


view : Model -> Html Msg
view model =
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
            , ( "background-position", animationFrame model )
            , ( "transform", spriteFacing model )
            , ( "transform-origin", "center bottom" )
            ]
        ]
        []
