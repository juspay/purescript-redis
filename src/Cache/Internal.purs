module Cache.Internal where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Data.Foreign (Foreign, readString)
import Data.Maybe (Maybe)
import Prelude (class Eq, class Show, Unit, pure, show, unit, ($), (/=), (<<<), (<>), (==))
import Test.Spec.Assertions (fail)

readStringMaybe :: Foreign -> Maybe String
readStringMaybe = hush <<< runExcept <<< readString

isNotZero :: Int -> Boolean
isNotZero x = x /= 0

checkValue :: forall a. Eq a => Show a => Either Error a -> a -> Aff _ Unit
checkValue eitherV exp = case eitherV of
                              Right v | v == exp -> pure unit
                              Right v'           -> fail $ "Bad value: " <> show v'
                              Left err           -> fail $ show err
