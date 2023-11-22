module Test.Hspec.Core.Tags where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.Hspec.Core.Spec hiding (pruneTree, pruneForest)
import           Test.Hspec.Core.Config
import           Test.Hspec.Core.Config.Definition (TagValue(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Dynamic

import           GetOpt.Declarative


useTags :: Spec
useTags = do
  modifyConfig ( \ config -> config { configTags = Map.insert "slow" (SetPending "slow item") $ configTags config } )
  modifyConfig ( \ config -> config { configMapSpecForest = \ c -> applyTagsToSpec c . configMapSpecForest config c } )
  modifyConfig ( \ config -> config { configOptions = tagOption : configOptions config } )


tag :: String -> SpecWith a -> SpecWith a
tag name = mapSpecItem_ $ \ item -> item { itemTags = toDyn name : itemTags item }

applyTagsToSpec :: Config -> [SpecTree ()] -> [SpecTree ()]
applyTagsToSpec config = filterForest pTag . bimapForest id setPending
  where
    setPending :: Item () -> Item ()
    setPending item = item { itemExample = \ params hook progress -> foldl' setPendingFoo (itemExample item params hook progress) ouao }
      where
        setPendingFoo :: IO Result -> (String, String) -> IO Result
        setPendingFoo result (name, reason)
          | name `elem` tags = return $ Result "" (Pending Nothing $ Just reason)
          | otherwise  = result

        tags :: [String]
        tags = mapMaybe fromDynamic $ itemTags item

        ouao :: [(String, String)]
        ouao = [(name, reason) | (name, SetPending reason) <- config_tags]

    config_tags = Map.toList $ configTags config

    pTag :: Item a -> Bool
    pTag = (||) <$> itemIsFocused <*> mk_p_tag

    mk_p_tag :: Item a -> Bool
    mk_p_tag item = and $ map mk_p config_tags
      where
        mk_p :: (String, TagValue) -> Bool
        mk_p (name, value) = case value of
          Select -> name `elem` tags
          Discard -> name `notElem` tags
          SetPending _ -> True

        tags :: [String]
        tags = mapMaybe fromDynamic $ itemTags item

option :: String -> OptionSetter config -> String -> Option config
option name arg help = Option name Nothing arg help True

argument :: String -> (String -> Maybe a) -> (a -> Config -> Config) -> OptionSetter Config
argument name parser setter = Arg name $ \ input c -> flip setter c <$> parser input

tagOption :: Option Config
tagOption = option "tags" (argument "TAGS" return addTags) "XXXXXXXXXXXXXXXX TODO XXXXXXXXXXXXX"

addTags :: String -> Config -> Config
addTags input c = c { configTags = foldl' (\ tags tag_ -> insertTag tag_ tags) (configTags c) (parseTags input) }

insertTag :: (String, Maybe TagValue) -> Map String TagValue -> Map String TagValue
insertTag (name, new) = Map.alter f name
  where
    f :: Maybe TagValue -> Maybe TagValue
    f _ = new

parseTags :: String -> [(String, Maybe TagValue)]
parseTags = map parseTag . words

parseTag :: String -> (String, Maybe TagValue)
parseTag input = case input of
  '-' : name -> (name, Just Discard)
  '+' : name -> (name, Nothing)
  name -> (name, Just Select)
