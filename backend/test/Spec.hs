module Main where

import Test.Hspec
import Test.Hspec.Attoparsec

import Data.Text (Text)
import Data.Text.IO (readFile)
import Parser
import Prelude hiding (readFile)

main :: IO ()
main = do
  nodeDot <- readFile "./test/node.dot"
  edgeDot <- readFile "./test/edge.dot"
  hspec $
    describe "Parser" $ do
      describe "hashAndName" $
        it "works on glibc" $
          ("/nix/store/2kcrj1ksd2a14bm5sky182fv2xwfhfap-glibc-2.26-131" :: Text) ~> hashAndName
            `shouldParse` ("2kcrj1ksd2a14bm5sky182fv2xwfhfap", "glibc-2.26-131")
      describe "nixPath" $ do
        it "works on glibc" $
          nixPath `shouldSucceedOn`
          ("/nix/store/2kcrj1ksd2a14bm5sky182fv2xwfhfap-glibc-2.26-131" :: Text)
        it "works without /nix/store" $
          nixPath `shouldSucceedOn`
          ("2kcrj1ksd2a14bm5sky182fv2xwfhfap-glibc-2.26-131" :: Text)
        it "fails on illegal characters" $
          nixPath `shouldFailOn`
          ("/nix/store/2kcrj\"ksd2a14bm5sky182fv2xwfhfap-glibc-2.26-131" :: Text)
      describe "quoted" $
        it "can parse a nixPath in quotes" $
          ("\"/nix/store/2kcrj1ksd2a14bm5sky182fv2xwfhfap-glibc-2.26-131\"" :: Text) ~> quoted nixPath
            `shouldParse` "/nix/store/2kcrj1ksd2a14bm5sky182fv2xwfhfap-glibc-2.26-131"
      describe "dotNode" $ do
        it "can parse a DOT node" $
          dotNode `shouldSucceedOn` nodeDot
        it "fails on a DOT edge" $
          dotNode `shouldFailOn` edgeDot
      describe "dotEdge" $ do
        it "can parse a DOT edge" $
          dotEdge `shouldSucceedOn` edgeDot
        it "fails on a DOT node" $
          dotEdge `shouldFailOn` nodeDot
      describe "depGraph" $
        it "can parse zlib dependencies DOT" $
          (depGraph `shouldSucceedOn`) =<< readFile "./test/zlib-deps.dot"
