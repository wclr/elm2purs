module Elm.Module exposing (..)


import Html exposing (..)

import Browser exposing (something)
import Events


type X = A | B


type alias T = {}


float : Float
float = 10


fn : Float -> Html msg
fn = 10


-- multi-line signature fn : Float
fn2 : 
    Float -> Html msg
fn2 = 10


tupleFn : 
    a -> (Html msg, a)
tupleFn val = (div [] [], String.toInt val)


{-| Extract the RGBA (red, green, blue) 
-}
toRgba : Color -> { red : Float, green : Float, blue : Float }
toRgba Rgba r g b =
    { red = r, green = g, blue = b }


type alias Rgb = 
    { red : Float
    , green : Float
    , blue : Float 
    }


toRgba2 : Color -> Rgb
toRgba2 Rgba red g b =
    { red = 
        (*) red (1 // 2)
    , green = g
    , blue = b 
    }
