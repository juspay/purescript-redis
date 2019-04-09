module Test.Multi where

import Prelude

import Cache (CacheConn)
import Cache.Multi (execMulti, getHashKeyMulti, newMulti, setHashMulti) as C
import Data.Array (index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

multiTest :: CacheConn -> Spec _ Unit
multiTest cacheConn =
  describe "Multi" do
     it "works" do
        let multi   = C.newMulti cacheConn
            multi'  = C.setHashMulti "myhash" "firstKey" "100" multi
            multi'' = C.getHashKeyMulti "myhash" "firstKey" multi'
        {-- val <- C.setKeyMulti "tt" "100" multi >>= C.setexKeyMulti "testing" "200" "1000" >>= C.incrMulti "testing" >>= C.getKeyMulti "tt" --}
        val <- C.execMulti multi''
        case (flip index 1 <$> val) of
             Right (Just "100") -> pure unit
             Right (Just v)     -> fail $ "Bad value: " <> v
             Right Nothing      -> fail $ "Did not get value"
             Left err           -> fail $ show err
