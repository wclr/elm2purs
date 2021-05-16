module Test.Main where


import Prelude

import Data.Array (foldl, length, range, index)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), joinWith, split, stripPrefix)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Main (convertDefault)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as Fs
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)


fixtureDir :: String
fixtureDir = "test/fixture"


type CompareError =
  { idx :: Int
  , fst :: Maybe String
  , snd :: Maybe String
  }


printCompareError :: CompareError -> String
printCompareError { idx, fst, snd } =
  joinWith "\n"
    [ ""
    , "Lines are not equal at index: " <> show idx
    , ""
    , fromMaybe "No line in original text"
        (append "Original line : " <$> snd)
    , fromMaybe "No line in target text"
        (append "Target line   : " <$> fst)
    ]


startsWith :: String -> String -> Boolean
startsWith prefix =
  isJust <<< stripPrefix (Pattern prefix)


ifSkipped :: Maybe String -> Boolean
ifSkipped str = fromMaybe true $ startsWith "-- |" <$> str


compareByLine :: String -> String -> Maybe CompareError
compareByLine fstText sndText =
  foldl fn Nothing (range 0 maxLen)
  where
  fn = \res idx ->
    let
      fst = index fstLines idx
      snd = index sndLines idx
    in case res of
      Just _ -> res
      Nothing
        | fst == snd || ifSkipped snd  -> Nothing
        | otherwise -> Just { idx, fst, snd}

  splitLines = split (Pattern "\n")
  fstLines = splitLines fstText
  sndLines = splitLines sndText
  maxLen = max (length fstLines) (length sndLines) - 1


testFile :: forall m. Monad m => String -> SpecT Aff Unit m Unit
testFile name = do

  it testName do
    elm <- Fs.readTextFile UTF8 $ fileP <> ".elm"
    purs <- Fs.readTextFile UTF8 $ fileP <> ".purs"
    compareLines (convertDefault elm) purs
  -- it testName2 do
  --   purs <- Fs.readTextFile UTF8 $ fileP <> ".purs"
  --   compareLines (convertDefault purs) purs
  where
  testName = "Should convert from .elm to .purs " <> name
  -- testName2 = "Should convert from .purs to .purs " <> name
  fileP = fixtureDir <> "/" <> name
  compareLines =
    \t1 t2 ->
      case compareByLine t1 t2 of
        Just result -> fail $ printCompareError result
        Nothing -> pure unit


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do

  describe "Fixture files compare" do
    testFile "File1"
