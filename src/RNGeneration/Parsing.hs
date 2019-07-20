module RNGeneration.Parsing where


-- import Control.Applicative ((<|>))
import Control.Exception (SomeException, catch)
import Control.Monad.Trans.Except
import Data.Aeson

import RNGeneration.Types


data Settings = Settings {
  allNames :: [AreaName],
  allAreas :: [Area]
}

instance Semigroup Settings where
  s1 <> s2 = Settings {
      allNames = allNames s1 <> allNames s2,
      allAreas = allAreas s1 <> allAreas s2
    }

instance Monoid Settings where
  mempty = Settings {
      allAreas = mempty,
      allNames = mempty
    }

data ParseReport = ParseReport {
  duplicateAreas :: [AreaName],     -- ^ Areas defined more than once
  nonExistentAreas :: [AreaName],   -- ^ Area names used, but not defined
  unusedReqs :: [Collectable],      -- ^ Reqs defined, but not used
  duplicateReqs :: [Collectable],   -- ^ Reqs defined more than once
  nonExistentReqs :: [Collectable], -- ^ Reqs used, but not defined
                                    --   (Req can be Item, Option or Req)
  unusedOptions :: [Option],        -- ^ Option defined, but not used
  parsedAreas :: [Area],            -- ^ Collection of succesfully parsed Areas
  parsedReqs :: [NamedRequirement]  -- ^ Collection of succesfully parsed Reqs
}

data SettingPart = AreaPart Area
                 | ReqPart NamedRequirement
                 | OptionPart Option

data NamedRequirement = NamedReq Collectable Requirement

instance FromJSON SettingPart where
  parseJSON = withObject "SettingPart" $ \o ->
      AreaPart <$> parseJSON (Object o)
    -- <|> OtherPart <$> parseJSON (Object o)

parseJSONFile :: FilePath -> IO (Either String ParseReport)
parseJSONFile fp = runExceptT $ do
    result <- ExceptT $ eitherDecodeFileStrict' fp `catch` \e ->
                            return . Left $ show (e :: SomeException)
    toParseReport result

toParseReport :: [SettingPart] -> ExceptT String IO ParseReport
toParseReport parts = throwE "Not implemented yet"
