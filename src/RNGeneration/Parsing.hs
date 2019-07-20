module RNGeneration.Parsing where


import Control.Exception (SomeException, catch)
import Control.Lens
import Control.Monad (forM_, unless, when)
import Control.Monad.State (MonadState, lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Aeson
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Text (unpack)

import RNGeneration.Types
import RNGeneration.Parsing.Types


parseJSONFile :: FilePath -> IO (Either String ParseResult)
parseJSONFile fp = runExceptT $ do
    result <- ExceptT $ eitherDecodeFileStrict' fp `catch` \e ->
                            return . Left $ show (e :: SomeException)
    let (_, ps) = runState (mkParseState result) mempty
    if noDuplicates ps
      then return $ ps ^. parseResult
      else printErrors ps

noDuplicates :: ParseState -> Bool
noDuplicates ps = ps ^. duplicateAreas   == []
               && null (ps ^. duplicateReqs)
               && ps ^. duplicateOptions == []

printErrors :: ParseState -> ExceptT String IO a
printErrors ps = do
    withDupes duplicateAreas $ \as -> do
      printErr "Duplicate Area names found."
      printAll as $ unpack . getAreaName
    withDupes duplicateReqs $ \rs -> do
      printErr "Duplicate Requirement names found."
      let reqName x = let NamedReq name _ = x in name
      printAll rs $ unpack . reqName
    withDupes duplicateOptions $ \os -> do
      printErr "Duplicate Options found."
      printAll os $ unpack . optionName
    throwE "Failed to parse input: duplicate definitions found"
  where withDupes f = forM_ $ mkMaybe $ ps ^. f
        mkMaybe [] = Nothing
        mkMaybe a = Just a
        print' :: String -> String -> ExceptT String IO ()
        print' x = lift . putStrLn . mappend x
        printErr = print' "[ERROR]: "
        printInfo = print' "[INFO]: "
        printAll :: [a] -> (a -> String) -> ExceptT String IO ()
        printAll xs f = forM_ (zip [1..] xs) $ \(i, x) ->
            printInfo $ show (i :: Int) <> ": " <> f x

mkParseState :: [SettingPart] -> State ParseState ()
mkParseState = mapM_ $ \case
    AreaPart a -> parseArea a
    ReqPart nr -> parseReq nr
    OptionPart opt -> parseOption opt

parseArea :: Area -> State ParseState ()
parseArea area@(Area name _itemConnect) = do
    duplicate <- name `isDuplicate` encounteredAreas
    when duplicate $ modifying duplicateAreas $ (:) name
    unless duplicate $ do
        modifying encounteredAreas $ HS.insert name
        modifying (parseResult . parsedAreas) $ (:) area

parseReq :: NamedRequirement -> State ParseState ()
parseReq nr@(NamedReq name _) = do
    duplicate <- name `isDuplicate` encounteredReqs
    when duplicate $ modifying duplicateReqs $ (:) nr
    unless duplicate $ do
        modifying encounteredReqs $ HS.insert name
        modifying (parseResult . parsedReqs) $ (:) nr

parseOption :: Option -> State ParseState ()
parseOption opt = do
    duplicate <- opt `isDuplicate` encounteredOptions
    when duplicate $ modifying duplicateOptions $ (:) opt
    unless duplicate $ do
        modifying encounteredOptions $ HS.insert opt
        modifying (parseResult . parsedOptions) $ (:) opt

isDuplicate :: (MonadState s m, Eq a, Hashable a)
            => a
            -> LensLike' (Const Bool) s (HS.HashSet a)
            -> m Bool
isDuplicate = flip uses . HS.member
