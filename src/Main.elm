module Main exposing (..)

import AnimationFrame
import Html exposing (..)
import Keyboard exposing (KeyCode)
import Model exposing (..)
import Update exposing (..)
import View exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        toSpaceMsg isDown keyCode =
            if keyCode == 32 then
                SetSpace isDown
            else
                Noop
    in
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs (toSpaceMsg True)
        , Keyboard.ups (toSpaceMsg False)
        ]


main : Program Int Model Msg
main =
    Html.programWithFlags
        { init = \highscore -> ( init highscore, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
