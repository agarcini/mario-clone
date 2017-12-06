module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import AnimationFrame
import Keyboard exposing (KeyCode)
import Array


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
    24


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
    600


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
      , blocks = initBlockList
      , colliding = []
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
    = BottomEdge Coordinates
    | TopEdge Coordinates
    | LeftEdge Coordinates
    | RightEdge Coordinates
    | None


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
    let
        collisions =
            blockOverlapCheck model.blocks model
    in
        ( model
            |> updateTick delta
            |> updateVelocity delta collisions
            |> updateLocation delta collisions
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


updateVelocity : Float -> List Collision -> Model -> Model
updateVelocity delta collisions model =
    { model
        | facing =
            case model.facing of
                Right 0 ->
                    Right 0

                Right velocityX ->
                    if hasRightCollision collisions then
                        Right 0
                    else if velocityX + (acceleration * delta) >= maxVelocityX then
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
                    if isGrounded model collisions then
                        Grounded
                    else
                        Falling 0

                Jumping velocityY ->
                    if velocityY - (gravity * delta) > 0 then
                        Jumping (velocityY - (gravity * delta))
                    else
                        Falling 0

                Falling velocityY ->
                    if isGrounded model collisions then
                        Grounded
                    else if velocityY + (gravity * delta) <= terminalVelocity then
                        Falling (velocityY + (gravity * delta))
                    else
                        Falling terminalVelocity
    }


updateLocation : Float -> List Collision -> Model -> Model
updateLocation delta collisions model =
    { model
        | locationX =
            -- if isBlockedOnSide model blockList then
            --     if rightIsColliding model blockList then
            --         findLeftSideWall (getRightCollisionCoordinates model blockList)
            --     else if leftIsColliding model blockList then
            --         findRightSideWall (getLeftCollisionCoordinates model blockList)
            -- else
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
                    if model.locationY <= 0 then
                        0
                    else
                        findTopOfPlatform (getBottomCollisionCoordinates collisions)

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


isGrounded : Model -> List Collision -> Bool
isGrounded model collisions =
    if model.locationY <= 0 || isBottomColliding collisions then
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


isBlockedOnSide : Model -> List Coordinates -> Bool
isBlockedOnSide model blockList =
    if rightIsColliding model blockList || leftIsColliding model blockList then
        True
    else
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
        ((marioOverlappingBottom || marioOverlappingTop) && (marioOverlappingRight || marioOverlappingLeft))


bottomOverlapCheck : Coordinates -> Model -> List Collision -> List Collision
bottomOverlapCheck coordinates model list =
    let
        marioBottomEdge =
            model.locationY

        marioOverlappingBottom =
            marioBottomEdge < (coordinates.y + blockSize) && marioBottomEdge > coordinates.y
    in
        if overlapCheck coordinates model && marioOverlappingBottom then
            BottomEdge coordinates :: list
        else
            list


topOverlapCheck : Coordinates -> Model -> List Collision -> List Collision
topOverlapCheck coordinates model list =
    let
        marioTopEdge =
            model.locationY + spriteHeight

        marioOverlappingTop =
            marioTopEdge < (coordinates.y + blockSize) && marioTopEdge > coordinates.y
    in
        if overlapCheck coordinates model && marioOverlappingTop then
            TopEdge coordinates :: list
        else
            list


leftOverlapCheck : Coordinates -> Model -> List Collision -> List Collision
leftOverlapCheck coordinates model list =
    let
        marioLeftEdge =
            model.locationX

        marioOverlappingLeft =
            marioLeftEdge < (coordinates.x + blockSize) && marioLeftEdge > coordinates.x
    in
        if overlapCheck coordinates model && marioOverlappingLeft then
            LeftEdge coordinates :: list
        else
            list


rightOverlapCheck : Coordinates -> Model -> List Collision -> List Collision
rightOverlapCheck coordinates model list =
    let
        marioRightEdge =
            model.locationX + spriteWidth

        marioOverlappingRight =
            marioRightEdge > coordinates.x && marioRightEdge < (coordinates.x + blockSize)
    in
        if overlapCheck coordinates model && marioOverlappingRight then
            RightEdge coordinates :: list
        else
            list


isOverlapping : Model -> Coordinates -> List Collision
isOverlapping model coordinates =
    let
        overlapsDetected =
            []
    in
        overlapsDetected
            |> bottomOverlapCheck coordinates model
            |> topOverlapCheck coordinates model
            |> leftOverlapCheck coordinates model
            |> rightOverlapCheck coordinates model


blockOverlapCheck : List Coordinates -> Model -> List Collision
blockOverlapCheck blockList model =
    List.concatMap (isOverlapping model) blockList


listNotEmpty : List Collision -> Bool
listNotEmpty list =
    case List.length list of
        0 ->
            False

        _ ->
            True


isBottomCollision : Collision -> Bool
isBottomCollision collision =
    case collision of
        BottomEdge _ ->
            True

        _ ->
            False


hasBottomCollision : List Collision -> Bool
hasBottomCollision list =
    if List.any isBottomCollision list then
        True
    else
        False


isTopCollision : Collision -> Bool
isTopCollision collision =
    case collision of
        TopEdge _ ->
            True

        _ ->
            False


hasTopCollision : List Collision -> Bool
hasTopCollision list =
    if List.any isTopCollision list then
        True
    else
        False


isRightCollision : Collision -> Bool
isRightCollision collision =
    case collision of
        RightEdge _ ->
            True

        _ ->
            False


hasRightCollision : List Collision -> Bool
hasRightCollision collisions =
    let
        bottomCoordinates : List Coordinates
        bottomCoordinates =
            List.filter isBottomCollision collisions
                |> List.filterMap extractCoordinates

        rightCollisions =
            List.filter isRightCollision collisions

        dontHaveBottomCollision : Collision -> Bool
        dontHaveBottomCollision rightCollision =
            bottomCoordinates
                |> List.all (\bottomCoordinate -> Just bottomCoordinate /= extractCoordinates rightCollision)
    in
        rightCollisions
            |> List.filter dontHaveBottomCollision
            |> Debug.log "rightCollisions"
            |> List.isEmpty
            |> not



-- if List.any isRightCollision collisions then
--     True
-- else
--     False
-- collisions
--     |> List.filter isRightCollision
--     |> extractCoordinates


isLeftCollision : Collision -> Bool
isLeftCollision collision =
    case collision of
        LeftEdge _ ->
            True

        _ ->
            False


hasLeftCollision : List Collision -> Bool
hasLeftCollision list =
    if List.any isLeftCollision list then
        True
    else
        False



-- findBottomCollisions : Model -> List Coordinates -> List Bool
-- findBottomCollisions model blockList =
--     List.map isBottomCollision (blockOverlapCheck blockList model)


isBottomColliding : List Collision -> Bool
isBottomColliding collisions =
    collisions
        |> List.map isBottomCollision
        |> List.any identity



-- bottomIsColliding : Model -> List Coordinates -> Bool
-- bottomIsColliding model blockList =
--     List.member True (findBottomCollisions model blockList)


findTopCollisions : Model -> List Coordinates -> List Bool
findTopCollisions model blockList =
    List.map isTopCollision (blockOverlapCheck blockList model)


topIsColliding : Model -> List Coordinates -> Bool
topIsColliding model blockList =
    List.member True (findTopCollisions model blockList)


findRightCollisions : Model -> List Coordinates -> List Bool
findRightCollisions model blockList =
    List.map isRightCollision (blockOverlapCheck blockList model)


rightIsColliding : Model -> List Coordinates -> Bool
rightIsColliding model blockList =
    List.member True (findRightCollisions model blockList)


findLeftCollisions : Model -> List Coordinates -> List Bool
findLeftCollisions model blockList =
    List.map isLeftCollision (blockOverlapCheck blockList model)


leftIsColliding : Model -> List Coordinates -> Bool
leftIsColliding model blockList =
    List.member True (findLeftCollisions model blockList)


findIndexOfLastTrue : List Bool -> Int
findIndexOfLastTrue list =
    case Array.get ((List.length list) - 1) (Array.fromList list) of
        Just True ->
            ((List.length list) - 1)

        Just False ->
            findIndexOfLastTrue (List.take ((List.length list) - 1) list)

        Nothing ->
            -1


findCollidingBlock : List Coordinates -> Int -> Coordinates
findCollidingBlock blockList index =
    Maybe.withDefault
        { x = 0, y = negate blockSize }
        (Array.get index (Array.fromList blockList))


getBottomCollisionCoordinates : List Collision -> Coordinates
getBottomCollisionCoordinates collisions =
    collisions
        |> List.filter isBottomCollision
        |> List.head
        |> Maybe.andThen extractCoordinates
        |> Maybe.withDefault { x = 0, y = negate blockSize }


extractCoordinates : Collision -> Maybe Coordinates
extractCoordinates collision =
    case collision of
        TopEdge coord ->
            Just coord

        BottomEdge coord ->
            Just coord

        LeftEdge coord ->
            Just coord

        RightEdge coord ->
            Just coord

        None ->
            Nothing



-- findBottomCollisions model blockList
--     |> findIndexOfLastTrue
--     |> findCollidingBlock blockList


getTopCollisionCoordinates : Model -> List Coordinates -> Coordinates
getTopCollisionCoordinates model blockList =
    findTopCollisions model blockList
        |> findIndexOfLastTrue
        |> findCollidingBlock blockList


getRightCollisionCoordinates : Model -> List Coordinates -> Coordinates
getRightCollisionCoordinates model blockList =
    findRightCollisions model blockList
        |> findIndexOfLastTrue
        |> findCollidingBlock blockList


getLeftCollisionCoordinates : Model -> List Coordinates -> Coordinates
getLeftCollisionCoordinates model blockList =
    findLeftCollisions model blockList
        |> findIndexOfLastTrue
        |> findCollidingBlock blockList


findBottomOfPlatform : Coordinates -> Float
findBottomOfPlatform coordinates =
    coordinates.y


findTopOfPlatform : Coordinates -> Float
findTopOfPlatform coordinates =
    coordinates.y + (blockSize - 0.25)


findLeftSideWall : Coordinates -> Float
findLeftSideWall coordinates =
    coordinates.x


findRightSideWall : Coordinates -> Float
findRightSideWall coordinates =
    coordinates.y + (blockSize - 0.25)


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


initBlockList : List Coordinates
initBlockList =
    [ { x = 100, y = 18 }
    , { x = 100 + blockSize, y = 18 }
    , { x = 100 + (blockSize * 2), y = 18 }
    , { x = 100 + (blockSize * 3), y = 18 }
    , { x = 100 + (blockSize * 10), y = 0 }
    , { x = 100 + (blockSize * 3), y = 18 + (blockSize * 3) }
    , { x = 100 + (blockSize * 4), y = 18 + (blockSize * 3) }
    , { x = 100 + (blockSize * 5), y = 18 + (blockSize * 3) }
    , { x = 100 + (blockSize * 6), y = 18 + (blockSize * 5) }
    , { x = 100 + (blockSize * 7), y = 18 + (blockSize * 5) }
    , { x = 100 + (blockSize * 9), y = 18 + (blockSize * 7) }
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
              -- , attribute "data-colliding" (toString (bottomIsColliding model blockList))
            , attribute "data-jumping" (toString (model.jumping))
            , attribute "data-collisions" (toString (blockOverlapCheck model.blocks model))
            ]
            (character model :: List.map blockView model.blocks)


view : Model -> Html Msg
view model =
    worldMap model
