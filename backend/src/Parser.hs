module Parser where

import           Control.Monad                  ( fail )
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.Map                      as M
import qualified Data.Text                     as T
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
legalNixFileNameChar = inClass "a-zA-Z0-9+._-"

hashAndName :: Parser (Text, Text)
hashAndName = do
  _    <- string "/nix/store/"
  hash <- takeWhile legalNixHashChar
  when (T.length hash /= 32) $ fail "failed to parse hash"
  _    <- char '-'
  name <- takeWhile legalNixFileNameChar
  return (hash, name)

nixPath :: Parser Text
nixPath = do
  (hash, name) <- hashAndName
  return $ "/nix/store/" <> hash <> "-" <> name

quoted :: Parser a -> Parser a
quoted p = char '"' *> p <* char '"'

-- Given the output of `nix-store --query --graph`,
-- produce a tree of Nix store paths
depTree :: Parser DepTree
depTree = do
  _     <- string "digraph G {" *> restOfLine
  edges <- many (dotEdge <|> dotNode)
  _     <- string "}"
  return $ foldl build M.empty edges
 where
  build :: DepTree -> (Text, [Text]) -> DepTree
  build acc (from, to) = M.insertWith (++) from to acc

dotNode :: Parser (Text, [Text])
dotNode = do
  name <- quoted nixPath <* string " [" <* restOfLine
  return (name, [])

dotEdge :: Parser (Text, [Text])
dotEdge = do
  to   <- quoted nixPath
  _    <- string " -> "
  from <- quoted nixPath
  _    <- restOfLine
  return (from, [to])

-- Given the output of `nix why-depends --all $from $to`,
-- produce a list of reasons why `from` directly depends on `to`.
-- The output of `why-depends` will print the shortest paths first,
-- which is why we only need to parse the first level of indentation until
-- the first "=> "
whyDepends :: Parser [Why]
whyDepends = do
  restOfLine
  many why
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
