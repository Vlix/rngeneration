module RNGeneration (
    module RNGeneration.Types
  , module RNGeneration.Util
  ) where

import RNGeneration.Types
import RNGeneration.Util

{-
Actions to take in order:

* Parse file (ParseResult)
* Check for clashing names/cycles
* Make set of all used collectables/options/namedreqs
* Expand and smush NamedReqs
* Expand NamedReqs in all Connectors while constructing ErrorReport
* (smush all reqs? maybe option?)
*

-}

-- checkParseResult :: ParseResult -> Either [String] ParseResult
-- checkParseResult pr = do


-- resultToSettings :: ParseResult -> Maybe Settings
