module Parser where

import           Data.Attoparsec.Text
import           Data.Char
import           Data.Tree
import qualified Data.Text                     as T
import           Protolude               hiding ( hash
                                                )
import           Types


parseEither :: Parser a -> Text -> Either Text a
parseEither parser text = case parseOnly parser text of
  Right a   -> Right a
  Left  err -> Left $ toS err


restOfLine :: Parser ()
restOfLine = takeTill isEndOfLine *> endOfLine


nixPath :: Parser Text
nixPath = do
  store <- string "/nix/store"
  rest  <- takeTill isSpace
  return $ store <> rest


-- "/nix/store/2kcrj1ksd2a14bm5sky182fv2xwfhfap-glibc-2.26-131"
-- -> ("2kcrj1ksd2a14bm5sky182fv2xwfhfap", "glibc-2.26-131")
hashAndName :: Parser (Text, Text)
hashAndName = do
  _    <- string "/nix/store/"
  hash <- takeTill (== '-')
  _    <- char '-'
  name <- takeTill isSpace
  return (hash, name)


-- Given the output of `nix-store --query --tree`,
-- produce a tree of Nix store paths
depTree :: Parser (Tree Text)
depTree = do
  (_, rootLabel) <- node
  subForest      <- makeForest <$> many node
  return Node {..}
 where
  node :: Parser (Int, Text)
  node = do
    indent <- takeTill (== '/')
    label  <- takeWhile1 (not . isSpace) <* restOfLine
    let level = T.length indent `div` 4
    return (level, label)

  makeForest :: [(Int, Text)] -> [Tree Text]
  makeForest [] = []
  makeForest ((level, label) : xs) =
    let
      (children, rest) = break (\(x, _) -> x <= level) xs
    in
      case children of
        [] -> []
        _ -> [ Node {rootLabel = label, subForest = makeForest children} ]
        ++ makeForest rest


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
    return Why {..}

  isIndent :: Char -> Bool
  isIndent c = c == ' ' || c == '║' || c == '╠' || c == '╚' || c == '═'


size :: Parser Int
size = decimal
