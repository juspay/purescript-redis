module Test.Multi where

import Prelude

import Cache (CacheConn, SetOptions(..))
import Cache.Multi (execMulti, getHashKeyMulti, getKeyMulti, newMulti, setHashMulti, setMulti) as C
import Data.Array (index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

multiTest :: CacheConn -> Spec _ Unit
multiTest cacheConn =
  describe "Multi" do
     it "works" do
        let multi = C.newMulti cacheConn
                  # C.setMulti "mykey" "myvalue" Nothing NoOptions
                  # C.getKeyMulti "mykey"
                  # C.setHashMulti "myhash" "firstKey" "100"
                  # C.getHashKeyMulti "myhash" "firstKey"
        {-- val <- C.setKeyMulti "tt" "100" multi >>= C.setexKeyMulti "testing" "200" "1000" >>= C.incrMulti "testing" >>= C.getKeyMulti "tt" --}
        val <- C.execMulti multi
        checkValue val 1 "myvalue"
        checkValue val 3 "100"
  where
        checkValue res i value = case (flip index i <$> res) of
                                      Right (Just v) | v == value -> pure unit
                                      Right (Just v')             -> fail $ "Bad value: " <> v'
                                      Right Nothing               -> fail $ "Did not get value"
                                      Left err                    -> fail $ show err
