module RNGeneration.Parsing where


import Control.Arrow (left)
import Control.Exception (SomeException, catch)
import Control.Lens
import Control.Monad (forM_, when)
import Control.Monad.State (MonadState, lift)
import Control.Monad.Trans.Except hiding (catchE)
import Control.Monad.Trans.State
import Data.Aeson
import Data.Aeson.Internal (formatError)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Text (Text, intercalate, unpack)
import Data.Yaml
import Data.Yaml.Internal

import RNGeneration.Types
import RNGeneration.Parsing.Types


parseJSONFile :: FilePath -> IO (Either String ParseResult)
parseJSONFile fp = runExceptT $ do
    result <- catchE $ eitherDecodeFileStrict' fp
    handleParseState $ mkParseState result

parseYAMLFile :: FilePath -> IO (Either String ParseResult)
parseYAMLFile fp = runExceptT $ do
    (warnings, result) <- catchE $ left show <$> decodeFileWithWarnings fp
    ps <- if not $ null warnings
              then formatWarnings warnings
              else return $ mkParseState result
    handleParseState ps

catchE :: IO (Either String a) -> ExceptT String IO a
catchE f = ExceptT $ f `catch` \e -> return . Left $ show (e :: SomeException)

formatWarnings :: Monad m => [Warning] -> ExceptT String m a
formatWarnings = throwE . unlines . fmap go
  where go (DuplicateKey path) = formatError path "Duplicate key found"


handleParseState :: ParseState -> ExceptT String IO ParseResult
handleParseState ps
  | not $ noDuplicates ps = printErrors ps
  | otherwise = do
      let starts = ps ^. parseResult . parsedStart
          len = length starts
          msg = "More than one Start found. Found names: " <>
                    intercalate ", " (getAreaName <$> starts)
      when (len /= 1) $ throwE $ unpack msg
      return $ ps ^. parseResult

noDuplicates :: ParseState -> Bool
noDuplicates ps = null (ps ^. duplicateAreas)
               && null (ps ^. duplicateReqs)
               && null (ps ^. duplicateOptions)

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

mkParseState :: [SettingPart] -> ParseState
mkParseState = flip execState mempty . go
  where go = mapM_ $ \case
            AreaPart a -> parseArea a
            ReqPart nr -> parseReq nr
            OptionPart opt -> parseOption opt
            Start start -> parseStart start

parseArea :: Area Text -> State ParseState ()
parseArea area@(Area name _itemConnect) = do
    duplicate <- name `isDuplicate` encounteredAreas
    if duplicate then modifying duplicateAreas $ (:) name
      else do
        modifying encounteredAreas $ HS.insert name
        modifying (parseResult . parsedAreas) $ (:) area

parseReq :: NamedRequirement Text -> State ParseState ()
parseReq nr@(NamedReq name _) = do
    duplicate <- name `isDuplicate` encounteredReqs
    if duplicate then modifying duplicateReqs $ (:) nr
      else do
        modifying encounteredReqs $ HS.insert name
        modifying (parseResult . parsedReqs) $ (:) nr

parseOption :: Option -> State ParseState ()
parseOption opt = do
    duplicate <- opt `isDuplicate` encounteredOptions
    if duplicate then modifying duplicateOptions $ (:) opt
      else do
        modifying encounteredOptions $ HS.insert opt
        modifying (parseResult . parsedOptions) $ (:) opt

parseStart :: AreaName -> State ParseState ()
parseStart = modifying (parseResult . parsedStart) . (:)

isDuplicate :: (MonadState s m, Eq a, Hashable a)
            => a
            -> LensLike' (Const Bool) s (HS.HashSet a)
            -> m Bool
isDuplicate = flip uses . HS.member
