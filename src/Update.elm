port module Update exposing (Msg(..), update)

import Model exposing (GameState(..), HoleHeight, Model, addTree, init)
import Random
import Shared exposing (..)
import Time exposing (Time)


type alias Seconds =
    Float


type Msg
    = Tick Time
    | SpawnTree HoleHeight
    | SetSpace Bool
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ height, power, velocity, gameState, highscore } as model) =
    case msg of
        Tick time ->
            let
                delta =
                    Time.inSeconds (min time 25)
            in
            case gameState of
                Playing ->
                    model
                        |> step delta
                        |> spawnTree delta

                Waiting ->
                    ( { model
                        | height = model.height - delta * 750
                        , xPos = model.xPos + delta * 250
                      }
                    , saveHighscore highscore
                    )

        SpawnTree height ->
            ( model |> addTree height, Cmd.none )

        SetSpace isDown ->
            ( case gameState of
                Waiting ->
                    if isDown == False then
                        let
                            nextModel =
                                init highscore
                        in
                        { nextModel | gameState = Playing }
                    else
                        model

                Playing ->
                    { model
                        | space = isDown
                        , velocity =
                            if height == groundHeight && isDown == False then
                                power * 12.2 + 380
                            else
                                velocity
                        , gameState = Playing
                    }
            , Cmd.none
            )

        Noop ->
            ( model, Cmd.none )


step : Seconds -> Model -> Model
step delta model =
    model
        |> fillPower delta
        |> stepPlayer delta
        |> moveTrees delta
        |> moveGround delta
        |> collisionDetection


fillPower : Seconds -> Model -> Model
fillPower delta ({ space, height, power } as model) =
    let
        newPower =
            if space && height == groundHeight then
                power + (delta * 300)
            else
                0
    in
    { model | power = clamp 0 maxPower newPower }


stepPlayer : Seconds -> Model -> Model
stepPlayer delta ({ rotation, xPos, velocity, height } as model) =
    let
        nextRotation =
            rotation - (430 * delta)
    in
    { model
        | height = max groundHeight <| height + (velocity * delta)
        , xPos = min (xPos + delta * 60) playerXPos
        , velocity = velocity - (delta * 2750)
        , rotation =
            if nextRotation < -360 then
                nextRotation + 360
            else
                nextRotation
    }


moveTrees : Seconds -> Model -> Model
moveTrees delta ({ trees, score } as model) =
    let
        moveTree tree =
            { tree | xPos = tree.xPos - delta * gameSpeed }

        movedTrees =
            List.map moveTree trees

        diff =
            List.map (\tree -> playerXPos > tree.xPos + treeWidth)
    in
    { model
        | trees = movedTrees
        , score =
            if diff trees == diff movedTrees then
                score
            else
                score + 1
    }


moveGround : Seconds -> Model -> Model
moveGround delta model =
    let
        newPos =
            model.groundPosition - delta * gameSpeed

        groundPieceWidth =
            24
    in
    { model
        | groundPosition =
            if newPos < -groundPieceWidth then
                newPos + groundPieceWidth
            else
                newPos
    }


collisionDetection : Model -> Model
collisionDetection model =
    let
        treeTrunkBoxes { holeHeight, xPos } =
            let
                box top bottom =
                    { top = top
                    , bottom = bottom
                    , left = xPos
                    , right = xPos + treeWidth
                    }

                topTrunk =
                    box gameHeight (toFloat holeHeight)

                bottomTrunk =
                    box (toFloat holeHeight - holeSize) groundHeight
            in
            [ topTrunk
            , bottomTrunk
            ]

        allTrunkBoxes =
            List.concatMap treeTrunkBoxes model.trees

        playerBox =
            { top = model.height + playerHeight
            , bottom = model.height
            , left = playerXPos
            , right = playerXPos + playerWidth
            }

        collision player tree =
            player.left
                < tree.right
                && player.right
                > tree.left
                && player.top
                > tree.bottom
                && player.bottom
                < tree.top

        collided =
            List.any (collision playerBox) allTrunkBoxes
    in
    if collided then
        { model
            | gameState = Waiting
            , highscore = max model.score model.highscore
        }
    else
        model


spawnTree : Seconds -> Model -> ( Model, Cmd Msg )
spawnTree delta model =
    let
        spawnTime =
            2.6

        sinceSpawn =
            model.secondsSinceSpawn + delta

        newModel =
            { model | trees = List.filter (\tree -> tree.xPos > -treeWidth) model.trees }

        treeOffset =
            15
    in
    if sinceSpawn >= spawnTime then
        ( { newModel
            | secondsSinceSpawn = sinceSpawn - spawnTime
          }
        , Random.int
            (holeSize + groundHeight + treeOffset)
            (gameHeight - treeOffset)
            |> Random.generate SpawnTree
        )
    else
        ( { newModel | secondsSinceSpawn = sinceSpawn }, Cmd.none )


port saveHighscore : Int -> Cmd val
