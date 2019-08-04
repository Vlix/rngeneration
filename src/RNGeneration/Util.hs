{-# LANGUAGE BangPatterns #-}
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
smushReq req = case req of
    -- If it's just regular requirements, we're done.
    -- FIXME: expand NamedRequirements?
    Req{} -> req
    -- IMPOSSIBLE means just that, we done.
    IMPOSSIBLE -> IMPOSSIBLE
    -- Reduce two requirements in the AND way
    req1 :&& req2 -> smushReq req1 `andSmush` smushReq req2
    -- Reduce two requirements in the OR way
    req1 :|| req2 -> smushReq req1 `orSmush` smushReq req2

-- | Reducing requirements when combining with AND
andSmush :: (Hashable a, Eq a) => Requirement a -> Requirement a -> Requirement a
andSmush req1 req2 =
  case (req1, req2) of
    -- If any side is IMPOSSIBLE, it's impossible
    (IMPOSSIBLE, _) -> IMPOSSIBLE
    (_, IMPOSSIBLE) -> IMPOSSIBLE
    (Req r1, r2)
      -- If one side has no requirements,
      -- the other side are the requirements.
      | HS.null r1 -> r2
      -- Combine the requirements with nothing yet checked.
      | otherwise -> reqAND r1 mempty r2
    -- Keep singular requirements on the left side
    (_, Req{}) -> req2 `andSmush` req1

    -- Combining two ANDs that have regular Reqs can just be
    -- combined. Next 'andSmush' will clean this up further.
    (Req r1 :&& r2 , Req r3 :&& r4) ->
        -- We use :&& instead of 'andSmush' here,
        -- because we don't want this to go through reqAND again.
        combineEmpties $ Req (r1 <> r3) :&&
          -- Seeing as r2 already has r1 removed, just remove r3.
          -- Conversely do the same with r4.
          (combineEmpties $ removeReqs r3 r2 :&& removeReqs r1 r4)
    -- combine r2/r3/r4 (r2 is also :||)
    (Req r1 :&& r2, r3 :|| r4) -> undefined -- FIXME: combine ANDs
    -- Keep anything with an intersection on the left side
    (_, Req{} :&& _) -> req2 `andSmush` req1

    -- These OR combinations
    (r1 :|| r2, r3 :|| r4) -> undefined -- FIXME: combine ORs
    -- Anything else can't be further reduced.
    (r1, r2) -> r1 :&& r2

-- | Combine a requirement set with other requirements
reqAND :: (Hashable a, Eq a) => HashSet a -> HashSet a -> Requirement a -> Requirement a
reqAND r1 checked = \case
    IMPOSSIBLE -> IMPOSSIBLE
    -- If the RHS is empty, then the requirements are r1
    Req r2 | HS.null r2 -> Req r1
    -- Combine 2 sets of requirement
    Req r2 -> Req (r1 <> r2)
    -- Combine 2 sets of requirement and try again
    -- but don't check the 'req' for the same set twice.
    Req r2 :&& req -> reqAND (r1 <> r2) r2 req
    req
      -- Nothing new to remove, we're done
      | HS.null diff -> Req r1 :&& req
      -- Remove the unchecked requirements
      | otherwise -> case removeReqs diff req of
                        IMPOSSIBLE -> IMPOSSIBLE
                        -- The result is empty, so just r1
                        Req r | HS.null r -> Req r1
                        -- Combine sets
                        Req r -> Req (r1 <> r)
                        -- Unable to reduce any further
                        r -> Req r1 :&& r
  where diff
          | HS.null checked = r1
          | otherwise = r1 `HS.difference` checked

-- | Removes any reqs that are in the set.
removeReqs :: (Hashable a, Eq a) => HashSet a -> Requirement a -> Requirement a
removeReqs s x = case x of
    IMPOSSIBLE -> IMPOSSIBLE
    Req r2 -> Req $ r2 `HS.difference` s
    r1 :&& r2 -> combineEmpties $ removeReqs s r1 :&& removeReqs s r2
    r1 :|| r2 -> combineEmpties $ removeReqs s r1 :|| removeReqs s r2

-- | Combines compounds in case empty sets show up.
combineEmpties :: Requirement a -> Requirement a
combineEmpties !x = case x of
    Req r1 :&& r2 | HS.null r1 -> r2
    r1 :&& Req r2 | HS.null r2 -> r1
    Req r1 :|| _  | HS.null r1 -> Req HS.empty
    _ :|| Req r2  | HS.null r2 -> Req HS.empty
    req -> req

{-
-- | Only returns True on empty Req set
unsafeNull :: Requirement a -> Bool
unsafeNull (Req a) = HS.null a
unsafeNull _ = False
-}

-- Anything combined with OR needs to also be reduced.
orSmush :: (Hashable a, Eq a) => Requirement a -> Requirement a -> Requirement a
orSmush req1 req2 =
  case (req1, req2) of
    -- IMPOSSIBLE on either side can be removed
    (IMPOSSIBLE, r) -> r
    (r, IMPOSSIBLE) -> r
    (Req r1, r2) -> reqOR r1 r2
    (_     , Req{}) -> req2 `orSmush` req1
    -- FIXME: What do with intersections? (Req r1 :&& r2)
    (r1, r2) -> r1 :|| r2

reqOR :: (Hashable a, Eq a) => HashSet a -> Requirement a -> Requirement a
reqOR r1 = \case
    IMPOSSIBLE -> Req r1
    Req r2 -> combineReqsOR r1 r2
    Req r2 :&& r3 ->
      let isect = r1 `HS.intersection` r2
      in if r1 == isect then Req r1
            else combineEmpties $
              Req isect :&& (combineEmpties $ Req (r1 `HS.difference` isect)
                                                :|| removeReqs isect (Req r2 :&& r3))
    -- FIXME: unfinished case

-- | Any requirements that appear on both sides
-- can be changed to an AND with those requirements.
combineReqsOR :: (Hashable a, Eq a) => HashSet a -> HashSet a -> Requirement a
combineReqsOR r1 r2
  | HS.null isect = Req r1 :|| Req r2
  | otherwise = Req isect :&& (Req req1 :|| Req req2)
  where isect = r1 `HS.intersection` r2
        req1 = r1 `HS.difference` isect
        req2 = r2 `HS.difference` isect



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
    addConnector (Connector aName $ Req HS.empty) from
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
