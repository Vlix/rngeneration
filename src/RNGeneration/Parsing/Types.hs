{-# LANGUAGE TemplateHaskell #-}
module RNGeneration.Parsing.Types where


import Control.Applicative ((<|>))
import Control.Lens (makeLenses)
import Data.Aeson
import Data.DList as DL (DList, toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Graph (SCC(..), stronglyConnComp)
import Data.Text (Text)

import RNGeneration.Types
import RNGeneration.Util (getReqs)


-- | Cleaned up Settings after parsing (from ParseResult)
data Settings a = Settings {
  startArea  :: AreaName,              -- ^ Area to begin
  areaMap    :: HashMap AreaName (Area a), -- ^ All Areas (Map)
  allAreas   :: HashSet AreaName,      -- ^ All Areas (Set)
  allItemLocations :: HashSet AreaName, -- ^ All Areas with 'Item'
  allCollectables :: AllCollectables,
  -- ^ All Collectables.
  -- N.B. MVP uses the collectables defined in Areas and reshuffles.
  -- Later, the items and their amounts might be altered with extra
  -- settings or maybe also at runtime.
  reqMap     :: ReqMap a,              -- ^ Map of pre-defined requirements
  allOptions :: AllOptions             -- ^ All known Options

  -- N.B. Not sure about the following fields,
  -- but something like these should be somewhere.
  -- Probably runtime tracking types.

  -- configuredOptions :: HashSet Option -- ^ Flagged runtime options (add at runtime)
  -- positiveReqs :: HashSet Text -- ^ All Collectables or Options that resolve to TRUE (probably runtime as well)
  -- openItemLocations :: HashSet AreaName
  -- -- ^ Used for placing random items. Remove AreaName after placing,
  -- -- and add new AreaNames when new ones open up.
}

type ReqMap a = HashMap Text (Requirement a)
type AllOptions = HashSet Option
type AllCollectables = HashSet Collectable

fromNamedReqs :: [NamedRequirement a] -> HashMap Text (Requirement a)
fromNamedReqs nReqs = HM.fromList [(name, req) | NamedReq name req <- nReqs]

findClashingNames :: ReqMap a -> AllCollectables -> AllOptions -> [Text]
findClashingNames reqMap collSet' optSet' =
    optColl <> optReq <> collReq
  where reqSet = HM.keysSet reqMap
        collSet = HS.map collectableName collSet'
        optSet = HS.map optionName optSet'
        optColl = foldIt "Collectable/Option: " $ optSet `HS.intersection` collSet
        optReq = foldIt "Requirement/Option: " $ optSet `HS.intersection` reqSet
        collReq = foldIt "Collectable/Requirement: " $ collSet `HS.intersection` reqSet
        foldIt t = HS.foldl' go []
          where go acc el = (mappend t) el : acc

-- | Find cycles in NamedRequirement definitions
reqMapCycles :: ReqMap Text -> [[Text]]
reqMapCycles reqMap =
    [g
    | CyclicSCC g <- stronglyConnComp
        [ (name, name, DL.toList $ getReqs req)
        | (name, req) <- HM.toList reqMap
        ]
    ]


data SettingPart = Start AreaName
                 | AreaPart (Area Text)
                 | ReqPart (NamedRequirement Text)
                 | OptionPart Option

instance FromJSON SettingPart where
  parseJSON = withObject "SettingPart" $ \o ->
      AreaPart <$> parseJSON (Object o)
    <|> ReqPart <$> parseJSON (Object o)
    <|> Start <$> o .: "start"
    <|> parseOption o
    where parseOption o = do
            typ <- o .: "type"
            case typ of
              "option" -> OptionPart <$> o .: "name"
              wat -> fail $ "Unrecognized \"type\" field: " <> wat
{-
data AllReqs = ReqItem Collectable
             | ReqReq Text
             | ReqOption Option
-}

data ErrorReport = ErrorReport {
  _nonExistentAreas :: [AreaName], -- ^ Area names used, but not defined
  _nonExistentReqs :: [Text],      -- ^ Reqs used, but not defined (Req = Item/Option/Req)
  _unusedReqs :: [NamedRequirement Text], -- ^ Reqs defined, but not used
  _unusedOptions :: [Option]       -- ^ Option defined, but not used
}
makeLenses ''ErrorReport

data ParseResult = ParseResult {
  _parsedStart :: [AreaName],
  _parsedAreas :: [Area Text],
  _parsedUsedAreas :: DList AreaName,
  -- ^ Found Areas in Connectors
  _parsedUsedCollectables :: [Collectable],
  -- ^ Found Collectabls in Connectors
  _parsedReqs :: [NamedRequirement Text],
  _parsedUsedReqs :: DList Text,
  -- ^ Found Reqs in Connectors
  _parsedOptions :: [Option]
}
makeLenses ''ParseResult

data ParseState = ParseState {
  _encounteredAreas :: HashSet AreaName, -- ^ Used in parsing to check duplicates
  _duplicateAreas   :: [AreaName],       -- ^ Areas defined more than once
  _encounteredReqs :: HashSet Text,      -- ^ Used in parsing to check duplicates
  _duplicateReqs :: [Text],              -- ^ Reqs defined more than once
  _encounteredOptions :: HashSet Option, -- ^ Used in parsing to check duplicates
  _duplicateOptions :: [Option],         -- ^ Options defined more than once
  _parseResult :: ParseResult
}
makeLenses ''ParseState


--------- SEMIGROUPS ---------
--------- SEMIGROUPS ---------
--------- SEMIGROUPS ---------

instance Semigroup ParseResult where
   pr1 <> pr2 = ParseResult {
      _parsedStart = _parsedStart pr1 <> _parsedStart pr2,
      _parsedAreas = _parsedAreas pr1 <> _parsedAreas pr2,
      _parsedUsedAreas = _parsedUsedAreas pr1 <> _parsedUsedAreas pr2,
      _parsedUsedCollectables = _parsedUsedCollectables pr1 <> _parsedUsedCollectables pr2,
      _parsedReqs = _parsedReqs pr1 <> _parsedReqs pr2,
      _parsedUsedReqs = _parsedUsedReqs pr1 <> _parsedUsedReqs pr2,
      _parsedOptions = _parsedOptions pr1 <> _parsedOptions pr2
    }

instance Semigroup ErrorReport where
  pr1 <> pr2 = ErrorReport {
      _nonExistentAreas = _nonExistentAreas pr1 <> _nonExistentAreas pr2,
      _nonExistentReqs = _nonExistentReqs pr1 <> _nonExistentReqs pr2,
      _unusedReqs = _unusedReqs pr1 <> _unusedReqs pr2,
      _unusedOptions = _unusedOptions pr1 <> _unusedOptions pr2
}

instance Semigroup ParseState where
  ps1 <> ps2 = ParseState {
      _encounteredAreas = _encounteredAreas ps1 <> _encounteredAreas ps2,
      _duplicateAreas = _duplicateAreas ps1 <> _duplicateAreas ps2,
      _encounteredReqs = _encounteredReqs ps1 <> _encounteredReqs ps2,
      _duplicateReqs = _duplicateReqs ps1 <> _duplicateReqs ps2,
      _encounteredOptions = _encounteredOptions ps1 <> _encounteredOptions ps2,
      _duplicateOptions = _duplicateOptions ps1 <> _duplicateOptions ps2,
      _parseResult = _parseResult ps1 <> _parseResult ps2
    }


--------- MONOIDS ---------
--------- MONOIDS ---------
--------- MONOIDS ---------

instance Monoid ParseResult where
  mempty = ParseResult mempty mempty mempty mempty mempty mempty mempty

instance Monoid ErrorReport where
  mempty = ErrorReport mempty mempty mempty mempty

instance Monoid ParseState where
  mempty = ParseState mempty mempty mempty mempty mempty mempty mempty
