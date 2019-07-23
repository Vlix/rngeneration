{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RNGeneration.Util (
    smushReq
  , addConnector
  , connectsTo
  , nubWith
) where


import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)

import RNGeneration.Types


-- | A way to remove unnecessary data constructors
-- and combine HashSets.
smushReq :: (Hashable a, Eq a) => Requirement a -> Requirement a
smushReq = rpReq . smushReq' . toFresh
-- RULE?: toFresh . rpReq == id
-- RULE?: toTried . rpReq == id

smushReq' :: (Hashable a, Eq a) => RequirementPhase a -> RequirementPhase a
smushReq' r@(RP req typ) =
    case typ of
      Tried -> r
      Fresh -> case req of
                Req{} -> r{rpType = Tried}
                IMPOSSIBLE -> r{rpType = Tried}
                req1 :&& req2 -> toFresh req1 `andSmush` toFresh req2
                req1 :|| req2 -> toFresh req1 `orSmush` toFresh req2

data RequirementPhase a = RP {
  rpReq :: Requirement a,
  rpType :: RequirementType
}

data RequirementType = Fresh | Tried
  deriving (Eq, Show)

toFresh :: Requirement a -> RequirementPhase a
toFresh r = RP r Fresh

toTried :: Requirement a -> RequirementPhase a
toTried r = RP r Tried

andSmush :: (Hashable a, Eq a) => RequirementPhase a -> RequirementPhase a -> RequirementPhase a
andSmush req1@(RP r1' Tried) req2@(RP r2' Tried) =
  case (r1', r2') of
    (IMPOSSIBLE, _) -> toTried IMPOSSIBLE
    (_, IMPOSSIBLE) -> toTried IMPOSSIBLE
    (Req r1  , Req r2) -> toTried $ Req $ r1 <> r2
    (_       , Req{} ) -> req2 `andSmush` req1
    (Req r1  , Req r2 :&& r3) -> toTried $ Req (r1 <> r2) :&& r3
    (Req r1 :&& r2 , Req r3 :&& r4) ->
        toTried $ Req (r1 <> r3) :&& r2 :&& r4
    (r1      , r2    ) -> toTried $ r1 :&& r2
andSmush req1 req2 =
    smushReq' req1 `andSmush` smushReq' req2

orSmush :: (Hashable a, Eq a) => RequirementPhase a -> RequirementPhase a -> RequirementPhase a
orSmush (RP r1' Tried) (RP r2' Tried) =
  case (r1', r2') of
    (IMPOSSIBLE, r) -> toTried r
    (r, IMPOSSIBLE) -> toTried r
    (Req r1, Req r2) ->
        let isect = r1 `HS.intersection` r2
            req1 = r1 `HS.difference` isect
            req2 = r2 `HS.difference` isect
            go = if isect == mempty then id else (Req isect :&&)
        in toTried $ go $ Req req1 :|| Req req2
    (r1, r2) -> toTried $ r1 :|| r2
orSmush req1 req2 = smushReq' req1 `orSmush` smushReq' req2



-- | Check for uniqueness before adding Connector to Area.
--
-- Behaves the following way:
-- * If the Area contains an Item, this returns Left
-- * If adding a Connector to an Area that is already
--   in the ConnectorMap, this combines the requirements with :|| (OR)
-- * Otherwise, this just adds the Connector to the ConnectorMap
addConnector :: (Hashable a, Eq a) => Connector AreaName a -> Area a -> Either Text (Area a)
addConnector c a = do
    mp <- case contains a of
            Item{} -> Left $ aName <> ": holds an item, can't add connector"
            Connect mp -> return mp
    let mFound = leadsTo c `HM.lookup` mp
    case mFound of
      Nothing -> return $ addConnectorUnsafe c a
      Just c' -> return $ addConnectorUnsafe c{requirements = newReqs c'} a
  where AreaName aName = areaName a
        newReqs c' = smushReq $ requirements c :|| requirements c'

-- | Adds a connector to an Area
-- NOTE: Does not check for duplicates (cf. 'addConnector')
--       and overwrites an Item.
addConnectorUnsafe :: (Hashable a, Eq a) => Connector AreaName a -> Area a -> Area a
addConnectorUnsafe c a = a{contains = newConnector}
  where newConnector = case contains a of
            Item{} -> Connect $ HM.singleton cName c
            Connect mp -> Connect $ HM.insert cName c mp
        cName = leadsTo c


-- | Connect first Area to second Area
connectsTo :: (Hashable a, Eq a) => Area a -> Area a -> Either Text (Area a)
connectsTo from to =
    addConnector (Connector aName $ Req mempty) from
  where aName = areaName to


------ MISCELLANEOUS ------
------ MISCELLANEOUS ------
------ MISCELLANEOUS ------

nubWith :: (Eq a, Hashable a) => [a] -> (HashSet a,[a])
nubWith = go mempty []
  where go found nubbed [] = (found, reverse nubbed)
        go found nubbed (a:as) = go newFound newNubbed as
          where (newFound, newNubbed)
                  | a `HS.member` found = (found, nubbed)
                  | otherwise = (HS.insert a found, a : nubbed)
