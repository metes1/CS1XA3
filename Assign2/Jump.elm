module Jump exposing (..)

import Html exposing (..)
import Html.Attributes as At
import Html.Events exposing (onClick)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Keyboard as Key


{- Model -}
type GameState = Menu | Play | Lose
type alias Model = { position : { x : Int, y : Int }, playerY : Int , state : GameState, score : Int, jump : Bool, obsDir : Bool, speed : Int }

{- View -}
--Styling for game screens
mStyle = At.style [("background-color", "#5A6378"), ("margin-right", "50px"), ("margin-left", "50px"), ("font-family", "courir"), ("color", "white"), ("text-align", "center"), ("padding", "5px")]
titleStyle = At.style [("font-size", "90px"), ("text-shadow", "0 0 20px #60B5CC")]
txtStyle = At.style [ ("font-size", "25px")]
buttonStyle = At.style [("display", "in-line"), ("border-radius", "36px"), ("color", "white"), ("font-size", "17px"), ("background", "#60B5CC"), ("font-family", "courir"), ("margin","20px"), ("padding", "10px 20px 10px 20px")]

--Main menu screen
menuScreen : Model -> Html.Html Msg
menuScreen model = div [mStyle]
                        [ h1 [titleStyle] [Html.text "Jump"]
                        , p [txtStyle] [Html.text "Objective: Jump over the square and don't get hit."]
                        , p [txtStyle] [Html.text "Controls: Press space bar to jump."]
                        , button [onClick ResetMsg, buttonStyle] [Html.text "Play Game"]
                        ]

--Screen where game is played
playStyle = At.style [("background-color", "#5A6378"), ("margin-top", "0px"), ("margin-right", "50px"), ("margin-left", "50px"), ("font-size", "50px"), ("color", "white"), ("text-align", "center")]
playScreen : Model -> Html.Html Msg
playScreen model = let
      posX = toString model.position.x
      posY = toString model.position.y
      playY = toString model.playerY
      scoreT = toString model.score
    in 
    --Draws shapes and background onto the Play screen
    div [playStyle]
        [ h1 [playStyle] [Html.text scoreT]
        , svg [width "1150",height "300"]
          [
            rect [x posX,y posY, height "50", width "50", fill "#60B5CC"] []
          , circle [cx "575", cy playY, r "30", fill "#7FD13B"] []
          ]
        ]  

--Gameover screen that appears when player dies
loseScreen : Model -> Html.Html Msg
loseScreen model = div [mStyle]
                           [ h1 [titleStyle] [Html.text "Game Over!"]
                           , p[txtStyle] [Html.text "Score:"]
                           , p[txtStyle] [Html.text (toString model.score)]
                           , button [onClick ResetMsg, buttonStyle] [Html.text "Play Again?"]
                           ]

--Initial values of the game
init : (Model,Cmd.Cmd Msg)
init = ({ position = {x = 0, y = 250}, playerY = 250, state = Menu, score = 0, jump = False, obsDir = True, speed = 10 },Cmd.none)

--Displays a particular screen depending on the state of the app
view : Model -> Html.Html Msg
view model =
      if model.state == Play
      then
         playScreen model
      else if model.state == Lose
      then
        loseScreen model
      else
        menuScreen model 

--Checks if the player and the obstacle have collided
hitCheck : Model -> Bool
hitCheck model = if (abs (model.position.x - 575) <= 40 ) && (abs (model.position.y - model.playerY) <= 20)
                 then
                    True
                 else
                    False

--Makes player go up when jumping and then preceeds to pull them back down to ground level
jump : Model -> Int
jump model =  if (model.jump == True && model.playerY >= 150)
              then
                model.playerY - 10
              else if (model.jump == False && model.playerY /= 270)
              then
                model.playerY + 10
              else
                model.playerY

--Changes the player's jump state depending on if they've reached their max jump height
jumpState : Model -> Bool
jumpState model = if (model.jump == True && model.playerY >= 150)
                  then
                    True
                  else
                    False --Jump turns to false if player reaches max height, now they will fall back to the ground

--Keeps track of how many times the player consecutively jumps over the obstacle
scoreTracker : Model -> Int
scoreTracker model = if (model.position.x == 720 || model.position.x == 725) && model.obsDir == False
                     then
                        model.score + 1
                      else if (model.position.x == 420 || model.position.x == 425) && model.obsDir == True
                      then
                        model.score + 1
                      else
                        model.score

--Speeds up the obstacle depending on how high the score is
speedUp : Model -> Int
speedUp model = if model.score < 5
                     then
                        10
                     else if model.score >= 5 && model.score < 10 && model.position.x == 0
                     then
                        15
                     else if model.score >= 10 && model.score < 15 && model.position.x == 0
                     then
                        20
                     else if model.score >= 15 && model.score < 20 && model.position.x == 0
                     then
                        25
                     else if model.score >= 20 && model.position.x == 0
                     then
                        30
                     else 
                        model.speed

--Changes the direction of the moving obstacle, so it moves back and forth               
obstacleDir : Model -> Bool
obstacleDir model = if model.position.x <= 0 || model.position.x >= 1150
                    then
                      not model.obsDir
                    else
                      model.obsDir

{- Update -}
type Msg = Tick Float | ResetMsg | KeyMsg Key.KeyCode

update : Msg -> Model -> (Model,Cmd.Cmd Msg)
update msg model =
         case msg of
              --Resets game variables
              ResetMsg -> ({ position = {x = 0, y = 250}, playerY = 270, state = Play, score = 0, jump = False, obsDir = True, speed = 10 },Cmd.none)
              --If space key is pressed, the player jumps (jump variable set to true)
              (KeyMsg 32) -> 
                if model.playerY == 270 --Player can only jump when it's on ground level
                then 
                  ({ position = {x = model.position.x, y = 250}, playerY = 270, state = Play, score = model.score, jump = True, obsDir = model.obsDir, speed = model.speed },Cmd.none)
                else
                  ({ position = {x = model.position.x, y = 250}, playerY = model.playerY, state = Play, score = model.score, jump = False, obsDir = model.obsDir, speed = model.speed },Cmd.none)
              (KeyMsg _) -> (model, Cmd.none) --Pressing the other keys does nothing
              --Updates for each tick, enables animation of the shapes, and constantly checks the state of the game
              (Tick time) ->
                if model.state == Menu --Game is at Menu screen, variables are at their inital values
                then
                  ({ position = {x = 0, y = 250}, playerY = 270, state = Menu, score = 0, jump = False, obsDir = True, speed = 10 },Cmd.none)
                else 
                  if hitCheck model == True --Player dies if the square collides with them, lose screen appears
                  then
                    ({ position = {x = model.position.x, y = 250}, playerY = model.playerY, state = Lose, score = model.score, jump = False, obsDir = model.obsDir, speed = model.speed },Cmd.none)
                  else --If player is still alive, game variables are checked and updated
                    let
                      playY = jump model --Updates player's height
                      jumpS = jumpState model --Checks if player is jumping
                      scoreT = scoreTracker model --Updates score
                      dir = obstacleDir model --Updates obstacle direction
                      speedT = speedUp model --Update obstacle speed
                      posX = if obstacleDir model == False --Moves right
                             then
                                model.position.x + speedT
                             else if obstacleDir model == True --Moves left
                             then
                                model.position.x - speedT
                             else
                                0
                      modelN = { position = {x = posX, y = 250}, playerY = playY, state = Play, score = scoreT, jump = jumpS, obsDir = dir, speed = speedT}
                    in (modelN,Cmd.none)

{- Subscriptions -}
subscriptions: Model -> Sub Msg
subscriptions model = 
  if model.state == Play
  then
    Sub.batch
        [ Anim.times Tick
        , Key.downs KeyMsg
        ]
  else
    Sub.none

{- Main -}
main : Program Never Model Msg
main = Html.program
       {init = init,
        view = view,
        update = update,
        subscriptions = subscriptions }