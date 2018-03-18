module View exposing (view)

import Collage exposing (Form)
import Color
import Element exposing (Element)
import Html exposing (Html, div, node)
import Html.Attributes exposing (href, rel, style)
import Model exposing (GameState(..), Model, Tree)
import Shared exposing (..)
import Text
import Update exposing (Msg)


yOffset : Float
yOffset =
    gameHeight / 2


xOffset : Float
xOffset =
    gameWidth / 2


position : ( Float, Float ) -> Element -> Form
position ( x, y ) element =
    Element.container gameWidth gameHeight Element.bottomLeft element
        |> Collage.toForm
        |> Collage.move ( x, y )


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "auto" )
            ]
        ]
        [ [ Element.image gameWidth gameHeight "images/bg.png"
                |> Collage.toForm
          , movingGround model.groundPosition
          , power model.power
          ]
            ++ trees model.trees
            ++ [ text model
               , player model
               ]
            |> Collage.collage gameWidth gameHeight
            |> Element.toHtml
        ]


text : Model -> Form
text { score, highscore, gameState } =
    let
        txt size x y string color =
            Text.fromString string
                |> Text.typeface [ "Press Start 2P", "cursive" ]
                |> Text.height size
                |> Text.color color
                |> Collage.text
                |> Collage.move ( x, y )

        text size height string =
            Collage.group
                [ txt size 0 height string Color.black
                , txt size -3 (height + 3) string Color.white
                ]
    in
    case gameState of
        Playing ->
            text 50 (gameHeight / 4) (toString score)

        _ ->
            Collage.group
                [ text 25
                    ((gameHeight / 3) + 20)
                    "score"
                , text 25
                    ((gameHeight / 3) - 20)
                    (toString score)
                , text 25
                    ((gameHeight / 3) - 80)
                    "best"
                , text 25
                    ((gameHeight / 3) - 120)
                    (toString highscore)
                , text
                    25
                    ((gameHeight / 3) - 250)
                    "Press space"
                ]


movingGround : Float -> Form
movingGround groundPosition =
    Element.tiledImage (gameWidth + 50) 12 "images/moving_ground.png"
        |> Collage.toForm
        |> Collage.move ( groundPosition, -254 )


player : Model -> Form
player { height, rotation, gameState, xPos } =
    let
        img =
            case gameState of
                Playing ->
                    "frog1"

                Waiting ->
                    "frog2"
    in
    Element.image playerWidth playerHeight ("images/" ++ img ++ ".png")
        |> Collage.toForm
        |> Collage.rotate (degrees rotation)
        |> List.singleton
        |> Collage.collage playerWidth playerHeight
        |> position ( xPos, height )


power : Float -> Collage.Form
power power =
    let
        powerScale =
            (gameWidth - 32) / maxPower

        powerWidth =
            power * powerScale
    in
    Element.tiledImage (floor powerWidth) 20 "images/power.png"
        |> position ( 16, 16 )


trees : List Tree -> List Collage.Form
trees =
    List.map
        (\{ holeHeight, xPos } ->
            let
                treeEndPart =
                    Element.image treeWidth 4

                treeEndElement dir height =
                    Element.flow dir
                        [ Element.tiledImage treeWidth height "images/tree_trunk.png"
                        , treeEndPart "images/tree_end1.png"
                        , treeEndPart "images/tree_end2.png"
                        ]

                treeEndHeight =
                    8

                topTreeHeight =
                    gameHeight - holeHeight

                bottomTreeHeight =
                    gameHeight - topTreeHeight - holeSize - groundHeight

                topTree =
                    treeEndElement Element.down (topTreeHeight - treeEndHeight)

                bottomTree =
                    treeEndElement Element.up (bottomTreeHeight - treeEndHeight)
            in
            Element.flow Element.down
                [ topTree
                , Element.spacer treeWidth holeSize
                , bottomTree
                ]
                |> position ( xPos, groundHeight )
        )
