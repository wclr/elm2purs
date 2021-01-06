module Main (main) where

import Prelude hiding (apply)

import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty.Array
import Data.Either (fromRight)
import Data.Function (apply, applyFlipped)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replace, replaceAll)
import Data.String as String
import Data.String.Regex (Regex, regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Fiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process as Process
import Partial.Unsafe (unsafePartial)

-- | Article https://qiita.com/kimagure/items/39b26642b89bd87bf177
infixr 0 apply as <|
infixl 0 applyFlipped as |>

replaceExt :: String -> String
replaceExt name = 
  replace (Pattern ".elm") (Replacement ".purs") 
    <| name


reWord :: String -> Regex
reWord word = 
  -- the same as unsafeRegex
  unsafePartial 
    $ fromRight 
    $ regex ("\\b" <> word <> "\\b") global

reDecl :: Regex
reDecl = unsafeRegex "\n([\\S]+) :: (.*)\n\\S" global


extractTypeParams :: String -> String
extractTypeParams text = 
  text
  |> Regex.match (unsafeRegex "\\b[a-z1-9]+\\b" global)
  |> map NonEmpty.Array.toArray  
  |> fromMaybe []
  |> Array.catMaybes
  |> Array.slice 1 (-1)
  |> String.joinWith " "


insertTypeParams :: String -> String
insertTypeParams text =   
  case extractTypeParams text of
    "" -> text
    str -> String.replace 
      (Pattern "::") 
      (Replacement <| String.joinWith " " ["::", "forall", str <> "."])
      text
  

addTypeParams :: String -> String
addTypeParams text = 
  text 
  |> Regex.replace' reDecl (\str _ -> insertTypeParams str)


replaceUnicode :: String -> String
replaceUnicode text = 
  text
  |> String.replaceAll (Pattern ":: forall") (Replacement ":: ∀")


replaceText :: String -> String
replaceText text = 
  text 
  -- spaces 4 to 2
  |> replace (Pattern "    ") (Replacement "  ")
  -- module definition
  |> replace (Pattern "exposing (..)") (Replacement "")
  |> replace (Pattern ")") (Replacement ") where")
  |> replace (Pattern " exposing") (Replacement "")
  -- unqualified import
  |> replaceAll (Pattern "exposes (..)") (Replacement "")
  -- type declarations
  |> replaceAll (Pattern "type ") (Replacement "data ")    
  |> replaceAll (Pattern "data alias ") (Replacement "type ")
  -- type annotations and cons
  |> replaceAll (Pattern ":") (Replacement "::")  
  |> replaceAll (Pattern "::::") (Replacement ":")
  -- types
  |> Regex.replace (reWord "Float") "Number"
  |> Regex.replace (reWord "Bool") "Boolean"


main :: Effect (Fiber Unit)
main =
  launchAff do
    args <- liftEffect $ Process.argv
    case Array.head $ Array.drop 2 args of
      Nothing -> liftEffect <| log "Not input: path/to/File.elm"

      Just name -> do
        let toName = (replaceExt name)
        text <- FS.readTextFile UTF8 name
        liftEffect
          $ log
          $ "Converting " <> name <> " ==> " <> toName
        FS.writeTextFile UTF8 toName
          $ replaceUnicode
          $ addTypeParams 
          $ replaceText text
