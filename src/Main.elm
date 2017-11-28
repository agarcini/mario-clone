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


spriteHeight : Float
spriteHeight =
    27


blockSize : Float
blockSize =
    32


acceleration : Float
acceleration =
    0.75


gravity : Float
gravity =
    1.333


maxVelocityX : Float
maxVelocityX =
    150


jumpVelocity : Float
jumpVelocity =
    450


terminalVelocity : Float
terminalVelocity =
    -- 600
    10


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


facingRight : String
facingRight =
    "scale(1)"


facingLeft : String
facingLeft =
    "scale(-1,1)"


init : ( Model, Cmd Msg )
init =
    ( { locationX = 0
      , locationY = 0
      , facing = Right 0
      , jumping = Grounded
      , tick = 0
      , blocks = blockList
      , colliding = [ BottomEdge ]
      }
    , Cmd.none
    )


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
    , blocks : List Coordinates
    , colliding : List Collision
    }


type Collision
    = BottomEdge
    | TopEdge
    | LeftEdge
    | RightEdge


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
            ( { model
                | jumping =
                    case isAirborne model.jumping of
                        False ->
                            Jumping jumpVelocity

                        True ->
                            model.jumping
              }
            , Cmd.none
            )

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


isFalling : Jumping -> Bool
isFalling jumping =
    case jumping of
        Jumping _ ->
            False

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
            facingRight

        Left _ ->
            facingLeft


character : Model -> Html Msg
character model =
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


type alias Coordinates =
    { x : Float
    , y : Float
    }


overlapCheck : Coordinates -> Model -> Bool
overlapCheck coordinates model =
    let
        marioBottomEdge =
            model.locationY

        marioLeftEdge =
            model.locationX

        marioRightEdge =
            model.locationX + spriteWidth

        marioTopEdge =
            model.locationY + spriteHeight

        marioOverlappingBottom =
            marioBottomEdge < (coordinates.y + blockSize) && marioBottomEdge > coordinates.y

        marioOverlappingRight =
            marioRightEdge > coordinates.x && marioRightEdge < (coordinates.x + blockSize)

        marioOverlappingLeft =
            marioLeftEdge < (coordinates.x + blockSize) && marioLeftEdge > coordinates.x

        marioOverlappingTop =
            marioTopEdge < (coordinates.y + blockSize) && marioTopEdge > coordinates.y
    in
        (marioOverlappingBottom || marioOverlappingTop) && (marioOverlappingRight || marioOverlappingLeft)


bottomCollisionCheck : Coordinates -> Model -> List Collision -> List Collision
bottomCollisionCheck coordinates model list =
    let
        marioBottomEdge =
            model.locationY + spriteHeight
    in
        if overlapCheck coordinates model && isFalling model.jumping then
            BottomEdge :: list
        else
            list


topCollisionCheck : Coordinates -> Model -> List Collision -> List Collision
topCollisionCheck coordinates model list =
    let
        marioTopEdge =
            model.locationY + spriteHeight
    in
        if marioTopEdge > coordinates.y then
            TopEdge :: list
        else
            list


leftCollisionCheck : Coordinates -> Model -> List Collision -> List Collision
leftCollisionCheck coordinates model list =
    let
        marioLeftEdge =
            model.locationX

        marioLeftWithinZone =
            marioLeftEdge < (coordinates.x + blockSize) && marioLeftEdge > coordinates.x
    in
        if marioLeftWithinZone then
            LeftEdge :: list
        else
            list


rightCollisionCheck : Coordinates -> (Model -> (List Collision -> List Collision))
rightCollisionCheck coordinates model list =
    let
        marioRightEdge =
            model.locationX + spriteWidth

        marioRightWithinZone =
            marioRightEdge > coordinates.x && marioRightEdge < (coordinates.x + blockSize)
    in
        if marioRightWithinZone then
            RightEdge :: list
        else
            list


isColliding : Model -> Coordinates -> List Collision
isColliding model coordinates =
    let
        collisionsDetected =
            []
    in
        collisionsDetected
            |> bottomCollisionCheck coordinates model
            |> topCollisionCheck coordinates model
            |> leftCollisionCheck coordinates model
            |> rightCollisionCheck coordinates model


blockCollisionCheck : List Coordinates -> Model -> List (List Collision)
blockCollisionCheck blockList model =
    List.map (isColliding model) blockList


blockView : Coordinates -> Html Msg
blockView coordinates =
    div
        [ style
            [ ( "display", "table" )
            , ( "width", toString blockSize ++ "px" )
            , ( "height", toString blockSize ++ "px" )
            , ( "position", "absolute" )
            , ( "left", toString coordinates.x ++ "px" )
            , ( "bottom", toString coordinates.y ++ "px" )
            , ( "background-color", "brown" )
            , ( "border-radius", "4px" )
            , ( "box-sizing", "border-box" )
            , ( "box-shadow", "inset 0 0 0 2px rgba(0,0,0,0.1)" )
            ]
        ]
        []


blockList : List Coordinates
blockList =
    [ { x = 100, y = 25 }
      -- , { x = 100 + blockSize, y = 25 }
      -- , { x = 100 + (blockSize * 2), y = 25 }
      -- , { x = 100 + (blockSize * 3), y = 25 }
    ]


worldMap : Model -> Html Msg
worldMap model =
    let
        worldScale =
            2
    in
        div
            [ style
                [ ( "transform", "scale(" ++ toString worldScale ++ ")" )
                , ( "width", toString (100 / worldScale) ++ "vw" )
                , ( "height", "100vh" )
                , ( "transform-origin", "left bottom" )
                , ( "overflow", "hidden" )
                , ( "background-color", "skyblue" )
                ]
            , attribute "data-collisions" (toString (blockCollisionCheck blockList model))
            ]
            (character model :: List.map blockView model.blocks)


view : Model -> Html Msg
view model =
    worldMap model
