{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module RNGeneration.Types where


import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text, intercalate)


data AreaTree a = AreaTree {
  treeName :: AreaName, -- ^ Name of the branch
  treeContents :: ItemOrConnect (AreaTree a) a -- ^ Contents of the tree
}

itemBranch :: AreaName -> Collectable -> AreaTree a
itemBranch an col = AreaTree an $ Item col

connBranch :: AreaName -> ConnectorMap (AreaTree a) a -> AreaTree a
connBranch an mp = AreaTree an $ Connect mp

--------
-- An Area either connects to other areas
-- or contains a collectable.
--------

-- | This is any area that connects other areas
-- or it contains an item.
data Area a = Area {
  areaName :: AreaName,
  contains :: ItemOrConnect AreaName a
}

instance FromJSON (Area Text) where
  parseJSON = withObject "Area" $ \o -> do
      typ <- o .: "type"
      areaName <- o .: "name"
      case typ :: String of
        "area" -> do
            connObj <- o .: "connects"
            let contains = Connect $ HM.mapWithKey Connector connObj
            return Area{..}
        "item" -> do
            contains <- Item <$> o .: "holds"
            return Area{..}
        wat -> fail $ "Unrecognized \"type\" field: " <> wat

data ItemOrConnect a b = Item Collectable
                       | Connect (ConnectorMap a b)

-- | A collection of Connectors to a certain Area
type ConnectorMap a b = HashMap AreaName (Connector a b)

-- | A connector links areas with optional requirements
data Connector a b = Connector {
  leadsTo :: a,
  requirements :: Requirement b
}

newtype AreaName = AreaName { getAreaName :: Text }
  deriving (FromJSON, FromJSONKey, Hashable)
  deriving newtype (Eq, Show, Ord)

-- | Something to collect/place in the world.
newtype Collectable = Collectable {
  collectableName :: Text
} deriving (FromJSON, Hashable)
  deriving newtype (Eq, Show, Ord)

-- | An option is the pretty much the same as a Collectable,
-- functionally. The difference is, these are maybe "obtained"
-- at runtime, instead of while traversing the world.
newtype Option = Option {
  optionName :: Text
} deriving (FromJSON, Hashable)
  deriving newtype (Eq, Show, Ord)

-- TODO: Needs verification function after parsing of
-- 'NamedRequirement Text'. Should error on circular dependencies.
data NamedRequirement a = NamedReq Text (Requirement a)


instance FromJSON (NamedRequirement Text) where
  parseJSON = withObject "Requirement Definition" $ \o -> do
    typ <- o .: "type"
    case typ of
      "requirement" -> do
          name <- o .: "name"
          definition <- o .: "definition"
          return $ NamedReq name definition
      wat -> fail $ "Unrecognized \"type\" field: " <> wat

-- | Requirements are defined by a set of collectables
-- or a con-/disjuction of such sets
data Requirement a = Req (HashSet a) -- ^ any Item/Option/NamedRequirement name
                   | Requirement a :|| Requirement a
                   | Requirement a :&& Requirement a
                   | IMPOSSIBLE

instance FromJSON (Requirement Text) where
  parseJSON Null = pure $ Req mempty
  parseJSON (Number n) = fail $ "Requirement can't be a number: " <> show n
  parseJSON (String txt) = pure . Req $ HS.singleton txt
  -- TODO: Might need to check for duplicates, or count them?
  parseJSON a@Array{} = Req . HS.fromList <$> parseJSON a
  parseJSON (Bool b) | b = pure $ Req mempty
                     | otherwise = pure IMPOSSIBLE
  parseJSON val = withObject
      "Requirement"
      (\o -> choice o <|> go o)
      val
    where choice o = do
              choices <- o .: "choice"
              when (null choices) $ fail errMsg
              let (c:cs) = Req . HS.singleton <$> choices
              return $ foldr (:||) c cs
          errMsg = "\"choices\" fields needs an array with at least one value"
          go :: Object -> Parser (Requirement Text)
          go o = do
            this <- o .: "this"
            mOR <- o .:? "or"
            case mOR of
              Just or' -> pure $ this :|| or'
              Nothing -> do
                  and' <- o .: "and"
                  pure $ this :&& and'
