module Test.Hspec.Core.Config.DefinitionSpec (spec) where

import           Prelude ()
import           Helper hiding (Discard)

import           Test.Hspec.Core.Config.Definition

spec :: Spec
spec = do
  describe "splitOn" $ do
    it "splits a string" $ do
      splitOn ',' "foo,bar,baz" `shouldBe` ["foo", "bar", "baz"]

    it "splits *arbitrary* strings" $ property $ do
      let
        string :: Gen String
        string = arbitrary `suchThat` p

        p :: String -> Bool
        p = (&&) <$> not . null <*> all (/= ',')

      forAll (listOf string) $ \ xs -> splitOn ',' (intercalate "," xs) `shouldBe` xs

  describe "parseTags" $ do
    it "" $ do
      parseTags "foo" `shouldBe` [("foo", Just Select)]

    it "" $ do
      parseTags "-foo" `shouldBe` [("foo", Just Discard)]

    it "" $ do
      parseTags "+foo" `shouldBe` [("foo", Nothing)]

    it "" $ do
      parseTags "foo bar -baz" `shouldBe` [("foo", Just Select), ("bar", Just Select), ("baz", Just Discard)]
