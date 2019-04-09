module Test.Basic where

import Cache (CacheConn, SetOptions(..), exists, get, set)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Prelude (class Eq, class Show, Unit, bind, discard, pure, show, unit, ($), (<>), (==))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

basicTest :: CacheConn -> Spec _ Unit
basicTest cacheConn =
  describe "Basic" do
     it "works" do
        v0 <- set cacheConn "test-key" "foo" (Just $ Milliseconds 100.0) NoOptions
        case v0 of
             Right _  -> pure unit
             Left err -> fail $ show err

        v4 <- exists cacheConn "test-key"
        checkValue v4 true

        v1 <- get cacheConn "test-key"
        checkValue v1 (Just "foo")

        delay $ Milliseconds 200.0

        v2 <- get cacheConn "test-key"
        checkValue v2 Nothing

        v3 <- exists cacheConn "test-key"
        checkValue v3 false

  where
        checkValue :: forall a. Eq a => Show a => Either Error a -> a -> Aff _ Unit
        checkValue eitherV exp = case eitherV of
                                      Right v | v == exp -> pure unit
                                      Right v'           -> fail $ "Bad value: " <> show v'
                                      Left err           -> fail $ show err
