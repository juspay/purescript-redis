module Test.List where

import Prelude

import Cache (CacheConn)
import Cache (lindex, lpop, rpush) as C
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

listTest :: CacheConn -> Spec _ Unit
listTest cacheConn =
  describe "List" do
     it "works" do
        v0 <- C.lpop cacheConn "test-list"
        checkValue v0 Nothing

        v1 <- C.rpush cacheConn "test-list" "hi"
        checkValue v1 1

        v2 <- C.lpop cacheConn "test-list"
        checkValue v2 (Just "hi")

        v3 <- C.lpop cacheConn "test-list"
        checkValue v3 Nothing

        l <- C.rpush cacheConn "DBACTIONS" "SELCT * FROM CUSTOMERS;"
        pop <- C.lpop cacheConn "DBACTIONS"
        peek <- C.lindex cacheConn "DBACTIONS" 0
        checkValue peek Nothing

        pure unit
  where
        checkValue :: forall a. Eq a => Show a => Either Error a -> a -> Aff _ Unit
        checkValue eitherV exp = case eitherV of
                                      Right v | v == exp -> pure unit
                                      Right v'           -> fail $ "Unexpected value: " <> (show v')
                                      Left err           -> fail $ show err
