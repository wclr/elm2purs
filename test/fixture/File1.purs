module Elm.Module where


import Prelude
import Html

import Browser (something)
import Events as Events
import Data.Int as Int
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))


data X = A | B


type T = {}


float :: Number
float = 10


fn :: ∀ msg. Number -> Html msg
fn = 10


-- multi-line signature fn : Float
fn2 :: ∀ msg.
  Number -> Html msg
fn2 = 10


tupleFn :: ∀ a msg.
  a -> Tuple (Html msg) a
tupleFn val = ((div [] []) /\ (Int.fromString val))


{-| Extract the RGBA (red, green, blue)
-}
toRgba :: Color -> { red :: Number, green :: Number, blue :: Number }
toRgba Rgba r g b =
  { red: r, green: g, blue: b }


type Rgb =
  { red :: Number
  , green :: Number
  , blue :: Number
  }


toRgba2 :: Color -> Rgb
toRgba2 Rgba red g b =
  { red:
    (*) red (1 `div` 2)
  , green: g
  , blue: b
  }
