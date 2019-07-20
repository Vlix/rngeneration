module RNGeneration.Parsing.Types where


import Control.Applicative ((<|>))
import Data.Aeson
import Data.HashSet (HashSet)
import Data.Text (Text)

import RNGeneration.Types


data SettingPart = AreaPart Area
                 | ReqPart NamedRequirement
                 | OptionPart Option

instance FromJSON SettingPart where
  parseJSON = withObject "SettingPart" $ \o ->
      AreaPart <$> parseJSON (Object o)
    <|> ReqPart <$> parseJSON (Object o)
    <|> parseOption o
    where parseOption o = do
            typ <- o .: "type"
            case typ of
              "option" -> OptionPart <$> o .: "name"
              wat -> fail $ "Unrecognized \"type\" field: " <> wat

data AllReqs = ReqItem Collectable
             | ReqReq NamedRequirement
             | ReqOption Option


data ParseResult = ParseResult {
  parsedReport :: Maybe ParseReport,
  parsedAreas :: [Area],
  parsedReqs :: [NamedRequirement]
}

type ParseReport = ()

data Settings = Settings {
  allNames :: [AreaName],
  allAreas :: [Area]
}

data ParseState = ParseState {
  areaState :: AreaState,
  reqState :: ReqState,
  optionState :: OptionState,
  parseResult :: ParseResult
}

data AreaState = AreaState {
  encounteredAreas :: HashSet AreaName, -- ^ Used in parsing to check duplicates
  duplicateAreas   :: [AreaName], -- ^ Areas defined more than once
  nonExistentAreas :: [AreaName], -- ^ Area names used, but not defined
  asParsedAreas    :: [Area]      -- ^ Collection of succesfully parsed Areas
}

data ReqState = ReqState {
  encounteredReqs :: HashSet Text,
  -- ^ Used in parsing to check duplicates
  unusedReqs :: [NamedRequirement],
  -- ^ Reqs defined, but not used
  duplicateReqs :: [(NamedRequirement, NamedRequirement)],
  -- ^ Reqs defined more than once
  nonExistentReqs :: [Text],
  -- ^ Reqs used, but not defined
  --   (Req can be Item, Option or Req)
  psParsedReqs :: [NamedRequirement]
  -- ^ Collection of succesfully parsed Reqs
}

data OptionState = OptionState {
  encounteredOptions :: HashSet Option, -- ^ Used in parsing to check duplicates
  unusedOptions      :: [Option], -- ^ Option defined, but not used
  osParsedOptions    :: [Option]  -- ^ Collection of succesfully parsed Options
}


--------- SEMIGROUPS ---------
--------- SEMIGROUPS ---------
--------- SEMIGROUPS ---------

instance Semigroup ParseResult where
   pr1 <> pr2 = ParseResult {
      parsedReport = parsedReport pr1 <> parsedReport pr2,
      parsedAreas = parsedAreas pr1 <> parsedAreas pr2,
      parsedReqs = parsedReqs pr1 <> parsedReqs pr2
    }

instance Semigroup Settings where
  s1 <> s2 = Settings {
      allNames = allNames s1 <> allNames s2,
      allAreas = allAreas s1 <> allAreas s2
    }

instance Semigroup ParseState where
  ps1 <> ps2 = ParseState {
      areaState = areaState ps1 <> areaState ps2,
      reqState = reqState ps1 <> reqState ps2,
      optionState = optionState ps1 <> optionState ps2,
      parseResult = parseResult ps1 <> parseResult ps2
    }

instance Semigroup AreaState where
  as1 <> as2 = AreaState {
      encounteredAreas = encounteredAreas as1 <> encounteredAreas as2,
      duplicateAreas = duplicateAreas as1 <> duplicateAreas as2,
      nonExistentAreas = nonExistentAreas as1 <> nonExistentAreas as2,
      asParsedAreas = asParsedAreas as1 <> asParsedAreas as2
    }

instance Semigroup ReqState where
  rs1 <> rs2 = ReqState {
      encounteredReqs = encounteredReqs rs1 <> encounteredReqs rs2,
      unusedReqs = unusedReqs rs1 <> unusedReqs rs2,
      duplicateReqs = duplicateReqs rs1 <> duplicateReqs rs2,
      nonExistentReqs = nonExistentReqs rs1 <> nonExistentReqs rs2,
      psParsedReqs = psParsedReqs rs1 <> psParsedReqs rs2
    }

instance Semigroup OptionState where
  os1 <> os2 = OptionState {
      encounteredOptions = encounteredOptions os1 <> encounteredOptions os2,
      unusedOptions = unusedOptions os1 <> unusedOptions os2,
      osParsedOptions = osParsedOptions os1 <> osParsedOptions os2
    }


--------- MONOIDS ---------
--------- MONOIDS ---------
--------- MONOIDS ---------

instance Monoid ParseResult where
  mempty = ParseResult mempty mempty mempty

instance Monoid Settings where
  mempty = Settings mempty mempty

instance Monoid ParseState where
  mempty = ParseState mempty mempty mempty mempty

instance Monoid AreaState where
  mempty = AreaState mempty mempty mempty mempty

instance Monoid ReqState where
  mempty = ReqState mempty mempty mempty mempty mempty

instance Monoid OptionState where
  mempty = OptionState mempty mempty mempty
