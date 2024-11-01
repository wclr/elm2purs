module Main (main, convert, convertDefault) where


import Prelude hiding (apply)

import Data.Array (catMaybes, filter, foldl, length, slice, splitAt)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty.Array
import Data.Foldable (and, minimum)
import Data.FoldableWithIndex (findWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), Replacement(..), joinWith, replace, replaceAll, stripPrefix, stripSuffix, trim)
import Data.String as S
import Data.String.CodeUnits (countPrefix)
import Data.String.Regex (Regex)
import Data.String.Regex as R
import Data.String.Regex.Flags (global, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Fiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process as Process


-- STRING UTILS


startsWith :: String -> String -> Boolean
startsWith prefix =
  isJust <<< stripPrefix (Pattern prefix)


endsWith :: String -> String -> Boolean
endsWith suffix =
  isJust <<< stripSuffix (Pattern suffix)


trimEnd :: String -> String
trimEnd = R.replace re ""
  where
  re = unsafeRegex "\\s+$" noFlags


-- CONVERT UTILS


replaceExt :: String -> String
replaceExt name =
  replace (Pattern ".elm") (Replacement ".purs")
    $ name


reWord :: String -> Regex
reWord word =
  unsafeRegex ("\\b" <> word <> "\\b") global


extractTypeParams :: String -> String
extractTypeParams text =
  text
    -- negative look ahead for type signature " : "
    # R.match (unsafeRegex "\\b[a-z1-9]+\\b(?! :)" global)
    # map NonEmpty.Array.toArray
    # fromMaybe []
    # Array.catMaybes
    # Array.nub
    -- # Array.slice 1 (-1)
    # S.joinWith " "


makeImport :: String -> Maybe String -> Maybe String -> String
makeImport from what as =
  "import " <> from
    <> fromMaybe "" (append " " <$> what)
    <> fromMaybe "" (append " as " <$> as)


preludeImport :: String
preludeImport = makeImport "Prelude" Nothing Nothing


patternToModule :: String -> String -> (String -> Array String)
patternToModule pattern mod =
  \text ->
    if S.contains (Pattern $ pattern <> ".") text then [ makeImport mod Nothing (Just pattern) ]
    else []


importChecks :: Array (String -> Array String)
importChecks =
  [ patternToModule "Int" "Data.Int"
  , patternToModule "Bits" "Data.Int.Bits"
  , patternToModule "Number" "Data.Number"
  , patternToModule "String" "Data.String"
  , patternToModule "Set" "Data.Set"
  , patternToModule "Array" "Data.Array"
  , patternToModule "Maybe" "Data.Maybe"
  , patternToModule "Apply" "Control.Apply"
  , patternToModule "Tuple" "Data.Tuple"
  , patternToModule "CodeUnits" "Data.String.CodeUnits"
  , \text ->
      if S.contains (Pattern "Tuple") text then [ makeImport "Data.Tuple" (Just "(Tuple)") Nothing ]
      else []
  , \text ->
      if S.contains (Pattern "/\\") text then [ makeImport "Data.Tuple.Nested" (Just "((/\\), type (/\\))") Nothing ]
      else []

  , \text ->
      if S.contains (Pattern " : ") text then [ makeImport "Data.Array" (Just "((:))") Nothing ]
      else []
  , \text ->
      if S.contains (Pattern "Maybe") text then [ makeImport "Data.Maybe" (Just "(Maybe(..))") Nothing ]
      else []
  ]


replaceTypes :: String -> String
replaceTypes text =
  text
    # R.replace (reWord "\\(\\)") "Unit"
    # R.replace (reWord "Never") "Void"
    # R.replace (reWord "Float") "Number"
    # R.replace (reWord "Bool") "Boolean"
    # R.replace (reWord "List") "Array"
    # R.replace (reWord "Dict") "Map"


replaceBool :: String -> String
replaceBool text =
  text
    # R.replace (reWord "True") "true"
    # R.replace (reWord "False") "false"


replaceFns :: String -> String
replaceFns text =
  text
    # R.replace (reWord "String.toFloat") "Number.fromString"
    # R.replace (reWord "toFloat") "Int.toNumber"

    # R.replace (reWord "String.toInt") "Int.fromString"
    # R.replace (reWord "toInt") "Int.fromString"
    # R.replace (reWord "round ") "Int.round "

    # R.replace (reWord "String.toList") "CodeUnits.toCharArray"

    # R.replace (reWord "String.fromInt") "show"
    # R.replace (reWord "String.fromFloat") "show"
    # R.replace (reWord "String.isEmpty") "String.null"
    # R.replace (reWord "String.join") "String.joinWith"

    # R.replace (reWord "Maybe.map") "map"
    # R.replace (reWord "List.map") "map"
    # R.replace (reWord "Array.map") "map"
    # R.replace (reWord "List.append") "append"
    # R.replace (reWord "Array.append") "append"

    # R.replace (reWord "List.foldl") "Array.foldl"
    # R.replace (reWord "List.foldr") "Array.foldr"
    # R.replace (reWord "List.filter") "Array.filter"
    # R.replace (reWord "List.head") "Array.head"
    # R.replace (reWord "List.concat") "Array.concat"
    # R.replace (reWord "List.intersperse") "Array.intersperse"

    # R.replace (reWord "List.filterMap") "Array.mapMaybe"

    # R.replace (reWord "Dict.get") "Map.lookup"
    # R.replace (reWord "Dict.empty") "Map.empty"
    # R.replace (reWord "Dict.update") "(flip Map.alter)"

    # R.replace (reWord "Set.fromList") "Set.fromFoldable"

    # R.replace (unsafeRegex "\\s\\bround\\b" global) " Int.round"

    # R.replace (unsafeRegex "Maybe.map(\\d)" global) "Apply.lift$1"

    # R.replace (reWord "isNaN") "Number.isNaN"

    # R.replace (reWord "String.left") "String.take"
    # R.replace (reWord "String.dropLeft") "String.drop"

    # R.replace (reWord "String.concat") "String.joinWith \"\""

    # R.replace (reWord "Maybe.withDefault") "Maybe.fromMaybe"
    # R.replace (reWord "withDefault") "Maybe.fromMaybe"

    # R.replace (reWord "Tuple.first") "Tuple.fst"
    # R.replace (reWord "Tuple.second") "Tuple.snd"

    # R.replace (reWord "Bitwise.and") "Bits.and"
    # R.replace (reWord "Bitwise.or") "Bits.or"
    # R.replace (reWord "Bitwise.shiftLeftBy") "(flip Bits.shl)"
    # R.replace (reWord "Bitwise.shiftLeft") "Bits.shl"
    # R.replace (reWord "Bitwise.shiftRightBy") "(flip Bits.shr)"
    # R.replace (reWord "Bitwise.shiftRight") "Bits.shr"
    # R.replace (reWord "Bitwise.shiftRightLogical") "Bits.zshr"
    # R.replace (reWord "Bitwise.shiftRightZfBy") "(flip Bits.zshr)"


-- Candidates for smart replacement:
-- Tuple.mapBoth =
--String.startWith = str = isJust $ String.stripPrefix (Pattern str)
-- shiftLeftBy


replaceOperators :: String -> String
replaceOperators text =
  text
    # replaceAll (Pattern "++") (Replacement "<>")
    # R.replace (unsafeRegex " ::" global) (" : ")
    # replaceAll (Pattern " <|") (Replacement " $")
    # replaceAll (Pattern " |>") (Replacement " #")
    # replaceAll (Pattern " <<") (Replacement " <<<")
    # replaceAll (Pattern " >>") (Replacement " >>>")
    # replaceAll (Pattern " //") (Replacement " `div`")


findIndexForImports :: Array String -> Int
findIndexForImports lines =
  result.idx

  where
  isModule = startsWith "module "
  isTop = not <<< startsWith " "
  result = foldl
    ( \r line ->
        { idx: if r.found then r.idx else r.idx + 1
        , found: r.found || (not isModule line) && (isTop line)
        }
    )
    { idx: 0, found: false }
    lines


addImports :: Array String -> Array String
addImports lines =
  lines
    # insertAfter firstImportIdx prelude
    # insertAfter (lastImportIdx + Array.length prelude + 1) qualifiedImports
  where
  insertAfter =
    \idx what target ->
      let
        { before, after } = splitAt idx target
      in
        before <> what <> after

  isOpen = not <<< R.test (unsafeRegex "as|\\(" noFlags)
  isImport = startsWith "import "

  noImpIdx = findIndexForImports lines

  firstImportIdx =
    fromMaybe noImpIdx $ Array.findIndex (isImport) lines
  lastImportIdx =
    fromMaybe noImpIdx $ Array.findLastIndex (isImport) lines

  isFirstImportOpen = fromMaybe false
    $
      Array.index
        lines
        firstImportIdx
        <#> (\s -> isImport s && isOpen s)

  text = joinWith "\n" lines

  prelude = [ preludeImport ]
    <> (if isFirstImportOpen then [] else [ "" ])

  qualifiedImports = Array.sort $ foldl
    (\items check -> items <> check text)
    [] importChecks


tupleStrArity :: Boolean -> Int -> String
tupleStrArity typeLevel len =
  case typeLevel of
    true -> if len > 2 then show len else ""
    false -> if len > 1 then show len else ""


replaceTuples :: Boolean -> String -> String
replaceTuples typeLevel text =
  R.replace' (unsafeRegex "\\( ([^(){}]+) \\)" global)
    ( \_ items ->
        ( \t ->
            if S.contains (Pattern ",") t then (replaceInner t)
            else wrapParens t
        )
          (joinWith "" $ (catMaybes $ items))
    )
    text
  where
  useOperator = true
  wrapParens = \str -> "(" <> str <> ")"

  replaceInner :: String -> String
  replaceInner =
    \tupInner ->
      tupInner
        # S.split (Pattern ", ")
        # map trim
        # map
            ( \str ->
                if (length $ S.split (Pattern " ") str) > 1 then "(" <> str <> ")"
                else str
            )
        # \items ->
            (wrapper $ length items) <> (joinWith joiner items)
              # \res ->
                  case useOperator of
                    true -> wrapParens res
                    false -> res
  -- case typeLevel of
  --   false -> wrapParens res
  --   true -> res

  wrapper =
    \len ->
      case not useOperator of
        true -> "Tuple" <> tupleStrArity typeLevel len <> " "
        false -> ""
  joiner = if useOperator then " /\\ " else " "


insertTypeParams :: String -> String -> String
insertTypeParams extractText text =
  case extractTypeParams extractText of
    "" -> text
    str -> S.replace
      (Pattern "::")
      (Replacement $ S.joinWith "" [ "::", " forall ", str <> "." ])
      text


replaceUnicode :: String -> String
replaceUnicode text =
  text
    # S.replaceAll
        (Pattern ":: forall")
        (Replacement ":: ∀")


-- This is simple and not recursive.
replaceRecordAssign :: String -> String
replaceRecordAssign text =
  R.replace' (unsafeRegex ("\\{[^|{}]+\\}") global)
    ( \whole _ -> R.replace (unsafeRegex " =(\\s?)" global) (":$1") whole

    ) text


-- Just simple: { combined | -->  combined {
replaceRecordMod :: String -> String
replaceRecordMod text =
  text #
    R.replace (unsafeRegex ("\\{ ([a-z][a-zA-z1-9']+) \\|") global)
      "$1 {"


-- CONVERT


data CodeLine
  = Module String
  | Import String
  | SingleLineComment String
  | MultiLineCommentSingle String
  | MultiLineCommentStart String
  | MultiLineCommentEnd String
  | Type String
  | TypeAlias String
  | FunSig String
  | FunTop String
  | NonTop String
  | Empty


unwrapLine :: CodeLine -> String
unwrapLine codeL =
  case codeL of
    Module str -> str
    Import str -> str
    SingleLineComment str -> str
    MultiLineCommentSingle str -> str
    MultiLineCommentStart str -> str
    MultiLineCommentEnd str -> str
    Type str -> str
    TypeAlias str -> str
    FunSig str -> str
    FunTop str -> str
    NonTop str -> str
    Empty -> ""


isIndentedLine :: CodeLine -> Boolean
isIndentedLine line = startsWith " " (unwrapLine line)


isEmptyLine :: CodeLine -> Boolean
isEmptyLine line =
  case line of
    Empty -> true
    _ -> false


isTopLevelNonEmpty :: CodeLine -> Boolean
isTopLevelNonEmpty line = not isIndentedLine line && not isEmptyLine line


getCodeLine :: String -> CodeLine
getCodeLine str
  | eq "" str = Empty
  | startsWith "--" str = SingleLineComment str
  | startsWith "{-" str && endsWith "-}" str = MultiLineCommentSingle str
  | startsWith "{-" str = MultiLineCommentStart str
  | endsWith "-}" str = MultiLineCommentEnd str
  | startsWith "module " str = Module str
  | startsWith "import " str = Import str
  | startsWith "type alias " str = TypeAlias str
  | startsWith "type " str = Type str
  | not startsWith " " str =
      if S.contains (Pattern " : ") str || endsWith " :" str then FunSig str
      else FunTop str
  | otherwise = NonTop str


isCommentStart :: CodeLine -> Boolean
isCommentStart cl = case cl of
  MultiLineCommentStart _ -> true
  MultiLineCommentSingle _ -> true
  _ -> false


isCommentEnd :: CodeLine -> Boolean
isCommentEnd cl = case cl of
  MultiLineCommentEnd _ -> true
  MultiLineCommentSingle _ -> true
  _ -> false


isSingleComment :: CodeLine -> Boolean
isSingleComment cl = case cl of
  SingleLineComment _ -> true

  _ -> false


type ConvertState =
  { result :: Array String
  -- , preludeAdded :: Boolean
  , isModuleBody :: Boolean
  , isCommented :: Boolean
  , isSignature :: Boolean
  , currentIndex :: Int
  , codeLines :: Array CodeLine
  }


--
initState :: Array CodeLine -> ConvertState
initState codeLines =
  { result: []
  , isModuleBody: false
  , isCommented: false
  , isSignature: false
  , currentIndex: 0
  --, preludeAdded: false
  , codeLines
  }


replaceImportedModules :: String -> String
replaceImportedModules text =
  text
    # replaceMod "Set" "Data.Set"
  where
  replaceMod what with =
    replace (Pattern $ "import " <> what) (Replacement $ "import " <> with)


replaceImport :: String -> String
replaceImport text =
  text
    # R.replace
        (unsafeRegex "import ([A-Za-z1-9.]+) as ([A-Za-z1-9.]+) exposing (\\(..\\))$" noFlags)
        "import $1\nimport $1 as $2"
    # R.replace
        (unsafeRegex "import ([A-Za-z1-9.]+) as ([A-Za-z1-9.]+) exposing (\\(.+\\))$" noFlags)
        "import $1 $3\nimport $1 as $2"
    # R.replace
        (unsafeRegex "import ([A-Za-z1-9.]+) exposing (\\(.+\\))$" noFlags)
        "import $1 $2\nimport $1 as $1"
    # R.replace (unsafeRegex "import ([A-Za-z1-9.]+)$" noFlags) "import $1 as $1"
    # replace (Pattern " exposing (..)") (Replacement "")
    # replace (Pattern " exposing") (Replacement "")
    # replaceImportedModules


replaceModule :: String -> String
replaceModule text =
  text
    # replace (Pattern "exposing (..)") (Replacement "where")
    # replace (Pattern "exposing") (Replacement "")
    # replace (Pattern " )") (Replacement " ) where")


isFunTop :: CodeLine -> Boolean
isFunTop =
  case _ of
    FunTop _ -> true
    _ -> false


isBodyTopLine :: CodeLine -> Boolean
isBodyTopLine =
  case _ of
    Module _ -> false
    Import _ -> false
    NonTop _ -> false
    SingleLineComment _ -> false
    MultiLineCommentSingle _ -> false
    MultiLineCommentStart _ -> false
    MultiLineCommentEnd _ -> false
    _ -> true


isSigStart :: CodeLine -> Boolean
isSigStart =
  case _ of
    FunSig _ -> true
    Type _ -> true
    TypeAlias _ -> true
    _ -> false


getTextToNextTopLine :: Int -> Array CodeLine -> Maybe String
getTextToNextTopLine currentIndex codeLines =
  map joinText nextTopIdx
  where
  res =
    (flip findWithIndex)
      codeLines
      \i codeL -> i > currentIndex && (isFunTop codeL)
  nextTopIdx = res <#> (\found -> found.index)
  joinText =
    \idx -> joinWith "\n"
      $ map unwrapLine
      $ slice currentIndex idx codeLines


replaceSig :: Int -> Array CodeLine -> String -> String
replaceSig idx lines text = text
  # S.replaceAll (Pattern ":") (Replacement "::")
  # insertTypeParams (fromMaybe text $ getTextToNextTopLine idx lines)


replaceTypeDefSymbol :: String -> String
replaceTypeDefSymbol text =
  text
    # R.replace (unsafeRegex " :(\\s|$)" noFlags) " ::$1"


getIndent :: String -> Int
getIndent str = countPrefix (eq ' ') str


getMinIndent :: Array String -> Int
getMinIndent list =
  fromMaybe 0
    $ minimum (filter (_ > 0) $ list <#> getIndent)


replaceIndent :: Array String -> Array String
replaceIndent list =
  if indent == 4 then
    -- spaces 4 to 2
    list <#> (replaceAll (Pattern "    ") (Replacement "  "))
  else
    list
  where
  indent = getMinIndent list


replaceText :: String -> String
replaceText text =
  text
    # trimEnd


foldConvert :: ConvertState -> CodeLine -> ConvertState
foldConvert state codeL =
  state
    { result = state.result <> addLines
    , currentIndex = state.currentIndex + 1
    , isSignature = isSignature
    , isCommented = isCommented
    , isModuleBody = isModuleBody
    -- , preludeAdded = state.preludeAdded || addPrelude
    }
  where
  isSignature =
    case state.isSignature of
      false
        | isSigStart codeL -> true
        | otherwise -> false
      true -> not (isTopLevelNonEmpty codeL && not isSigStart codeL)
  isModuleBody =
    if state.isModuleBody then true
    else isBodyTopLine codeL

  isCurrentLineComment =
    state.isCommented
      || isSingleComment codeL
      || isCommentStart codeL

  isCommented =
    case state.isCommented of
      true -> if isCommentEnd codeL then false else true
      false -> if isCommentStart codeL && not isCommentEnd codeL then true else false

  addLines = [ converted ]

  converted =
    case isCurrentLineComment of
      true -> unwrapLine codeL
      false ->
        case codeL of
          Module str ->
            replaceModule str
          Import str ->
            str #
              replaceImport
          FunSig str ->
            str
              # replaceSig state.currentIndex state.codeLines
              # replaceUnicode
              # replaceTypes
              # replaceTuples true
          Type str ->
            str
              # replaceAll (Pattern "type ") (Replacement "data ")
          TypeAlias str ->
            str
              # replaceAll (Pattern "type alias ") (Replacement "type ")

          _ ->
            (unwrapLine codeL)
              # replaceText
              # case isModuleBody of
                  true ->
                    (replaceTuples isSignature)
                      <<< case isSignature of
                          true ->
                            replaceTypeDefSymbol
                              <<< replaceTypes
                          false ->
                            replaceOperators
                              <<< replaceBool
                              <<< replaceFns
                  false -> replaceModule


type ConvertOptions =
  {}


defaultOptions :: ConvertOptions
defaultOptions = {}


convert :: ConvertOptions -> String -> String
convert _ text =
  state.result
    # replaceIndent
    # addImports
    # S.joinWith "\n"
    # replaceRecordAssign
    # replaceRecordMod

  where
  lines = trimEnd <$> S.split (Pattern "\n") text
  codeLines = lines <#> getCodeLine
  state = foldl foldConvert (initState codeLines) codeLines


convertDefault :: String -> String
convertDefault = convert defaultOptions


main :: Effect (Fiber Unit)
main =
  launchAff do
    args <- liftEffect $ Process.argv
    case Array.head $ Array.drop 2 args of
      Nothing -> liftEffect $ log "Not input: path/to/File.elm1"

      Just name -> do
        let toName = (replaceExt name)
        text <- FS.readTextFile UTF8 name
        liftEffect
          $ log
          $ "Converting " <> name <> " ==> " <> toName
        FS.writeTextFile UTF8 toName (convertDefault text)
