module RNGeneration.Parsing where


-- import Control.Applicative ((<|>))
import Control.Exception (SomeException, catch)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Aeson

import RNGeneration.Types
import RNGeneration.Parsing.Types


parseJSONFile :: FilePath -> IO (Either String ParseResult)
parseJSONFile fp = runExceptT $ do
    result <- ExceptT $ eitherDecodeFileStrict' fp `catch` \e ->
                            return . Left $ show (e :: SomeException)
    let (_, ps) = runState (mkParseState result) mempty
        pr = toParseResult ps
    return pr

toParseResult :: ParseState -> ParseResult
toParseResult = undefined

mkParseState :: [SettingPart] -> State ParseState ()
mkParseState = mapM_ $ \case
    AreaPart a -> parseArea a
    ReqPart nr -> parseReq nr
    OptionPart opt -> parseOption opt

parseArea :: Area -> State ParseState ()
parseArea (Area name has) = return ()

parseReq :: NamedRequirement -> State ParseState ()
parseReq nr = return ()

parseOption :: Option -> State ParseState ()
parseOption opt = return ()
