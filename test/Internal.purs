module Test.Internal where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Test.Spec.Assertions (fail)

checkValue :: forall a. Eq a => Show a => Either Error a -> a -> Aff Unit
checkValue eitherV exp = case eitherV of
                              Right v | v == exp -> pure unit
                              Right v'           -> fail $ "Bad value: " <> show v'
                              Left err           -> fail $ show err

checkRight :: forall a. Either Error a -> Aff Unit
checkRight eitherV = case eitherV of
                          Right _  -> pure unit
                          Left err -> fail $ show err