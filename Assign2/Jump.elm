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
type alias Model = { position : { x : Int, y : Int }, playerY : Int , state : GameState, score : Int, jump : Bool, obsDir : Bool }

{- View -}
--Main menu screen
mStyle = At.style [("background-color", "black"), ("font-size", "60px"), ("margin", "40px"), ("font-family", "courir"), ("color", "white"), ("text-align", "center")]
txtStyle = At.style [ ("font-size", "20px")]
menuScreen : Model -> Html.Html Msg
menuScreen model = div [mStyle]
                        [ h1 [mStyle] [Html.text "Jump"]
                        , p [txtStyle] [Html.text "Objective: Jump over the square and don't get hit."]
                        , p [txtStyle] [Html.text "Controls: Press space bar to jump."]
                        , button [onClick ResetMsg] [Html.text "Play Game"]
                        ]

--Screen where game is played
playStyle = At.style [("background-color", "black"), ("margin", "40px"), ("font-size", "30px"), ("color", "white"), ("text-align", "center")]
playScreen : Model -> Html.Html Msg
playScreen model = let
      posX = toString model.position.x
      posY = toString model.position.y
      playY = toString model.playerY
      scoreT = toString model.score
    in 
    --draws shapes and background onto the screen
    div [playStyle]
        [ h1 [playStyle] [Html.text scoreT]
        , svg [width "1150",height "300"]
          [
            rect [x posX,y posY, height "50", width "50", fill "lime"] []
          , circle [cx "575", cy playY, r "30", fill "#02F3FF"] []
          ]
        ]  

--Gameover screen that appears when player dies
loseScreen : Model -> Html.Html Msg
loseScreen model = div [mStyle]
                           [ h1 [mStyle] [Html.text "Game Over!"]
                           , p[mStyle] [Html.text "Score"]
                           , p [mStyle] [Html.text (toString model.score)]
                           , button [onClick ResetMsg] [Html.text "Play Again?"]
                           ]

--Initial values of the game
init : (Model,Cmd.Cmd Msg)
init = ({ position = {x = 0, y = 250}, playerY = 250, state = Menu, score = 0, jump = False, obsDir = True },Cmd.none)

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
              else if (model.jump == True && model.playerY < 150)
              then
                model.playerY
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
                    False

--Keeps track of how many times the player consecutively jumps over the obstacle
scoreTracker : Model -> Int
scoreTracker model = if model.position.x == 720 && model.obsDir == False
                     then
                        model.score + 1
                      else if model.position.x == 420 && model.obsDir == True
                      then
                        model.score + 1
                      else
                        model.score

--Changes the direction of the                     
obstacleDir : Model -> Bool
obstacleDir model = if model.position.x <= 0 || model.position.x >= 1150 --Changes direction if obstacles reaches end of screen
                    then
                      not model.obsDir
                    else
                      model.obsDir

{- Update -}
type Msg = Tick Float | ResetMsg | KeyMsg Key.KeyCode

update : Msg -> Model -> (Model,Cmd.Cmd Msg)
update msg model =
         case msg of
              ResetMsg -> ({ position = {x = 0, y = 250}, playerY = 270, state = Play, score = 0, jump = False, obsDir = True },Cmd.none)
              (KeyMsg 32) -> 
                if model.playerY == 270
                then 
                  ({ position = {x = model.position.x, y = 250}, playerY = 270, state = Play, score = model.score, jump = True, obsDir = model.obsDir },Cmd.none)
                else
                  ({ position = {x = model.position.x, y = 250}, playerY = model.playerY, state = Play, score = model.score, jump = False, obsDir = model.obsDir },Cmd.none)
              (KeyMsg _) -> (model, Cmd.none)
              (Tick time) ->
                if model.state == Menu
                then
                  ({ position = {x = 0, y = 250}, playerY = 270, state = Menu, score = 0, jump = False, obsDir = True },Cmd.none)
                else 
                  if hitCheck model == True
                  then
                    ({ position = {x = model.position.x, y = 250}, playerY = model.playerY, state = Lose, score = model.score, jump = False, obsDir = model.obsDir },Cmd.none)
                  else

                    let
                      playY = jump model
                      jumpS = jumpState model
                      scoreT = scoreTracker model
                      dir = obstacleDir model
                      posX = if obstacleDir model == False --Moves right
                             then
                                model.position.x + 20
                             else if obstacleDir model == True --Moves left
                             then
                                model.position.x - 20
                             else
                                0
                      modelN = { position = {x = posX, y = 250}, playerY = playY, state = Play, score = scoreT, jump = jumpS, obsDir = dir}
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