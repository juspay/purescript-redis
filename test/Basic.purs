module Test.Basic where

import Cache (CacheConn, SetOptions(..), set)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Prelude (Unit, bind, pure, show, unit, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

basicTest :: CacheConn -> Spec _ Unit
basicTest cacheConn =
  describe "Basic" do
     it "works" do
        v <- set cacheConn "test-key" "foo" (Just $ Milliseconds 100.0) NoOptions
        case v of
             Right _  -> pure unit
             Left err -> fail $ show err
