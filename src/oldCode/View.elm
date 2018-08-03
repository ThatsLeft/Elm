module View exposing (..)

import Html exposing (Html)
import Color exposing (Color)
import Collage exposing (..)
import Element exposing (..)
import Text
import String
import Block exposing (..)
import Game exposing (..)
import User exposing (..)

--initialize the window
w : number
w = 500
h : number
h = 900

--background Color
backgroundColor : Color.Color
backgroundColor =
  Color.black


view game =
  toHtml (
      collage w h
      [ (filled backgroundColor (rect w h))
      , (moveY -300 (filled (userColor) (rect userW userH )))
      , (moveX leftWall.x (filled ( Color.darkCharcoal (rect leftWall.w leftWall.h) )))
      , (moveX rightWall.x (filled ( Color.darkCharcoal (rect rightWall.w rightWall.h) )))
      ]
  )
