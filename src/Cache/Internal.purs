module Cache.Internal where

import Control.Monad.Except (runExcept)
import Data.Array (concat)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Foreign (Foreign, readString)
import Prelude (map, (/=), (<<<), (==))

readStringMaybe :: Foreign -> Maybe String
readStringMaybe = hush <<< runExcept <<< readString

isNotZero :: Int -> Boolean
isNotZero x = x /= 0

isOk :: String -> Boolean
isOk x = x == "OK"

flattenArr :: Array (Tuple String String) -> Array String
flattenArr = concat <<< map (\(Tuple field value) -> [field, value])