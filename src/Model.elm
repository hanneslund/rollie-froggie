module Model exposing (GameState(..), HoleHeight, Model, Tree, addTree, init)

import Shared exposing (..)


type GameState
    = Waiting
    | Playing


type alias HoleHeight =
    Int


type alias Tree =
    { holeHeight : HoleHeight
    , xPos : Float
    }


type alias Model =
    { gameState : GameState
    , highscore : Int
    , score : Int
    , space : Bool
    , power : Float
    , height : Float
    , xPos : Float
    , velocity : Float
    , rotation : Float
    , trees : List Tree
    , secondsSinceSpawn : Float
    , groundPosition : Float
    }


init : Int -> Model
init highscore =
    { gameState = Waiting
    , highscore = highscore
    , score = 0
    , space = False
    , power = 0
    , height = groundHeight
    , xPos = -100
    , velocity = 0
    , rotation = 0
    , trees = []
    , secondsSinceSpawn = 0
    , groundPosition = 0
    }


addTree : Int -> Model -> Model
addTree holeHeight model =
    { model | trees = Tree holeHeight gameWidth :: model.trees }
