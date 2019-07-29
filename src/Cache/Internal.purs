module Cache.Internal where

import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Foreign (Foreign, readString)
import Prelude ((/=), (<<<))

readStringMaybe :: Foreign -> Maybe String
readStringMaybe = hush <<< runExcept <<< readString

isNotZero :: Int -> Boolean
isNotZero x = x /= 0