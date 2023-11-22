module Test.Hspec.Core.TagsSpec (spec) where

import           Prelude ()
import           Helper hiding (Discard)

import           Test.Hspec.Core.Config.Definition
import           Test.Hspec.Core.Tags

spec :: Spec
spec = do
  describe "parseTags" $ do
    it "" $ do
      parseTags "foo" `shouldBe` [("foo", Just Select)]

    it "" $ do
      parseTags "-foo" `shouldBe` [("foo", Just Discard)]

    it "" $ do
      parseTags "+foo" `shouldBe` [("foo", Nothing)]

    it "" $ do
      parseTags "foo bar -baz" `shouldBe` [("foo", Just Select), ("bar", Just Select), ("baz", Just Discard)]
