module Main (main, spec) where

import Prelude
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Config.Definition
import Test.Hspec.Expectations
import Test.QuickCheck hiding (Discard)
import Control.Arrow
import qualified Data.Map as Map
import System.Environment

main :: IO ()
main = hspec spec

onlyOnCI :: String -> Spec
onlyOnCI name = do
  ci <- runIO $ lookupEnv "CI"
  case ci of
    Nothing -> modifyConfig ( \ config -> config { configTags = Map.insert name (SetPending "slow") $ configTags config } )
    Just _ -> return ()

spec :: Spec
spec = do
  onlyOnCI "slow"
  describe "reverse" $ do
    it "reverses a list" $ do
      reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    it "gives the original list, if applied twice" >>> tag "slow" $ property $
      \ xs -> (reverse . reverse) xs == (xs :: [Int])
