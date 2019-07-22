{-# LANGUAGE TemplateHaskell #-}
module RNGeneration.Parsing.Types where


import Control.Applicative ((<|>))
import Control.Lens (makeLenses)
import Data.Aeson
import Data.HashSet (HashSet)
import Data.Text (Text)

import RNGeneration.Types


data SettingPart = Start AreaName
                 | AreaPart Area
                 | ReqPart NamedRequirement
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

data AllReqs = ReqItem Collectable
             | ReqReq Text
             | ReqOption Option


data ParseReport = ParseReport {
  _nonExistentAreas :: [AreaName],   -- ^ Area names used, but not defined
  _nonExistentReqs :: [Text],        -- ^ Reqs used, but not defined (Req = Item/Option/Req)
  _unusedReqs :: [NamedRequirement], -- ^ Reqs defined, but not used
  _unusedOptions :: [Option]         -- ^ Option defined, but not used
}
makeLenses ''ParseReport

data ParseResult = ParseResult {
  _parsedStart :: [AreaName],
  _parsedAreas :: [Area],
  _parsedReqs :: [NamedRequirement],
  _parsedOptions :: [Option]
}
makeLenses ''ParseResult

data ParseState = ParseState {
  _encounteredAreas :: HashSet AreaName, -- ^ Used in parsing to check duplicates
  _duplicateAreas   :: [AreaName],       -- ^ Areas defined more than once
  _encounteredReqs :: HashSet Text,      -- ^ Used in parsing to check duplicates
  _duplicateReqs :: [NamedRequirement],  -- ^ Reqs defined more than once
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
      _parsedReqs = _parsedReqs pr1 <> _parsedReqs pr2,
      _parsedOptions = _parsedOptions pr1 <> _parsedOptions pr2
    }

instance Semigroup ParseReport where
  pr1 <> pr2 = ParseReport {
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
  mempty = ParseResult mempty mempty mempty mempty

instance Monoid ParseReport where
  mempty = ParseReport mempty mempty mempty mempty

instance Monoid ParseState where
  mempty = ParseState mempty mempty mempty mempty mempty mempty mempty
