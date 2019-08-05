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

-- TODO: Connectors probably need IDs as well, for
-- the developers to know which connector is which
-- (in case of entrance shuffle)
-- TODO: Also needs a setting to tell if this connector
-- __CAN/SHOULD__ be randomized or not.
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

-- FIXME: Use a tagged 'NamedRequirement ReqType' to ignore
-- anything that isn't a predefined requirement!
-- TODO: Good idea to add some unit tests for this to check
-- it actually catches circular dependencies.
checkDeps :: HashMap Text (NamedRequirement Text)
          -> NamedRequirement Text
          -> Either [Text] ()
checkDeps _reqMap (NamedReq txt req) = go [txt] (HS.singleton txt) req
  where go acc s = \case
            Req reqSet
              | clash <- s `HS.intersection` reqSet
              , clash /= mempty ->
                  let clashes = intercalate ", " $ HS.toList clash
                  in Left $ "(" <> clashes <> ")" : acc
              | otherwise ->
                  let _noClash = s `HS.intersection` reqSet
                  in undefined -- FIXME: look up named reqs and continue down
            r1 :|| r2 -> (<>) <$> go acc s r1 <*> go acc s r2
            r1 :&& r2 -> (<>) <$> go acc s r1 <*> go acc s r2
            _ -> Right ()


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
  deriving (Eq)

instance Show a => Show (Requirement a) where
  show = \case
    IMPOSSIBLE -> "IMPOSSIBLE"
    Req a -> show $ HS.toList a
    a :&& b -> "(" <> show a <> " AND " <> show b <> ")"
    a :|| b -> "(" <> show a <> " OR " <> show b <> ")"

instance FromJSON (Requirement Text) where
  parseJSON Null = pure $ Req mempty
  parseJSON (Number n) = fail $ "Requirement can't be a number: " <> show n
  parseJSON (String txt) = pure . Req $ HS.singleton txt
  -- TODO: Might need to check for duplicates, or count them?
  parseJSON a@Array{} = do
      adds <- parseJSON a
      if null adds
        then pure $ Req mempty
        else let (x:xs) = adds
             in pure $ foldl (:&&) (x :: Requirement Text) xs
  parseJSON (Bool b) | b = pure $ Req mempty
                     | otherwise = pure IMPOSSIBLE
  parseJSON val = withObject
      "Requirement"
      (\o -> choice o <|> go o)
      val
    where choice o = do
              choices <- o .: "choice"
              when (null choices) $ fail errMsg
              let (c:cs) = choices
              return $ foldl (:||) (c :: Requirement Text) cs
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
