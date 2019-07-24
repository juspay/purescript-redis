module Cache.Internal where

import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe)
import Effect.Aff (Aff, Error)
import Foreign (Foreign, readString)
import Prelude (class Eq, class Show, Unit, pure, show, unit, ($), (/=), (<<<), (<>), (==))
import Test.Spec.Assertions (fail)

readStringMaybe :: Foreign -> Maybe String
readStringMaybe = hush <<< runExcept <<< readString

isNotZero :: Int -> Boolean
isNotZero x = x /= 0

checkValue :: forall a. Eq a => Show a => Either Error a -> a -> Aff Unit
checkValue eitherV exp = case eitherV of
                              Right v | v == exp -> pure unit
                              Right v'           -> fail $ "Bad value: " <> show v'
                              Left err           -> fail $ show err

checkRight :: forall a. Either Error a -> Aff Unit
checkRight eitherV = case eitherV of
                          Right _  -> pure unit
                          Left err -> fail $ show err
