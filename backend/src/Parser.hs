module Parser where

import           Control.Monad                  ( fail )
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Maybe
import qualified Data.Map.Strict               as Map
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as Vector
import qualified Data.Text                     as Text
import           Protolude               hiding ( hash
                                                , takeWhile
                                                , try
                                                )
import           Types


parseEither :: Parser a -> Text -> Either Text a
parseEither parser text = case parseOnly parser text of
  Right a   -> Right a
  Left  err -> Left $ toS err

restOfLine :: Parser ()
restOfLine = takeTill isEndOfLine *> endOfLine

legalNixHashChar :: Char -> Bool
legalNixHashChar = inClass "a-zA-Z0-9"

legalNixFileNameChar :: Char -> Bool
legalNixFileNameChar = inClass "a-zA-Z0-9+.?=_-"

hashAndName :: Parser (Text, Text)
hashAndName = do
  _    <- string "/nix/store/"
  hash <- takeWhile legalNixHashChar
  when (Text.length hash /= 32) $ fail "failed to parse hash"
  _    <- char '-'
  name <- takeWhile legalNixFileNameChar
  return (hash, name)

nixPath :: Parser Text
nixPath = do
  (hash, name) <- hashAndName
  return $ "/nix/store/" <> hash <> "-" <> name

quoted :: Parser a -> Parser a
quoted p = char '"' *> p <* char '"'

-- Parse the output of `nix-store --query --graph`,
depGraph :: Parser DepGraph
depGraph = do
  _            <- string "digraph G {" *> restOfLine
  edgesOrNodes <- many (dotEdge <|> dotNode)
  _            <- string "}"
  let
    (n, e) =
      Vector.partition (isNothing . snd) . Vector.fromList $ edgesOrNodes
    nodes = Vector.map fst n

    addOne (numSoFar, acc) name = (numSoFar + 1, Map.insert name numSoFar acc)
    (_, index) = Vector.foldl addOne (0, Map.empty) nodes

    mkEdge (source, Just target) =
      Just (index Map.! source, index Map.! target)
    mkEdge _ = Nothing
    edges = Vector.map fromJust . Vector.filter isJust . Vector.map mkEdge $ e
  return $ DepGraph { .. }

dotNode :: Parser (Text, Maybe Text)
dotNode = do
  name <- quoted nixPath <* string " [" <* restOfLine
  return (name, Nothing)

dotEdge :: Parser (Text, Maybe Text)
dotEdge = do
  to   <- quoted nixPath
  _    <- string " -> "
  from <- quoted nixPath
  _    <- restOfLine
  return (from, Just to)

-- Given the output of `nix why-depends --all $from $to`,
-- produce a list of reasons why `from` directly depends on `to`.
-- The output of `why-depends` will print the shortest paths first,
-- which is why we only need to parse the first level of indentation until
-- the first "=> "
whyDepends :: Parser (Vector Why)
whyDepends = do
  _ <- nixPath *> string "\n"
  whys <- choice
    [ why `manyTill` arrow
    , return []
    ]
  return $ Vector.fromList whys
 where
  -- `filepath:…reason…` => Why
  why :: Parser Why
  why = do
    skipWhile isIndent
    file   <- takeTill (== ':') <* takeTill (== '…') <* char '…'
    reason <- takeTill (== '…')
    restOfLine
    return Why { .. }

  isIndent :: Char -> Bool
  isIndent c = c == ' ' || c == '║' || c == '╠' || c == '╚' || c == '═'

  arrow :: Parser ()
  arrow = do
    skipWhile isIndent
    _ <- string "=> "
    restOfLine

-- Given the output of `nix path-info --size --closure-size $path`,
-- get size and closure size
sizeAndClosureSize :: Parser (Int, Int)
sizeAndClosureSize = do
  _ <- nixPath
  skipSpace
  size <- decimal
  skipSpace
  closureSize <- decimal
  restOfLine
  return (size, closureSize)
