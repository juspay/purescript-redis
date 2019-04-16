module Test.Multi where

import Prelude

import Cache (CacheConn, SetOptions(..), get)
import Cache.Multi (execMulti, expireMulti, getMulti, hgetMulti, hsetMulti, newMulti, setMulti)
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

multiTest :: CacheConn -> Spec _ Unit
multiTest cacheConn =
  describe "Multi" do
     it "works" do
        multi <- liftEff $
           newMulti cacheConn
           >>= setMulti "mykey" "myvalue" Nothing NoOptions
           >>= getMulti "mykey"
           >>= hsetMulti "myhash" "firstKey" "100"
           >>= hgetMulti "myhash" "firstKey"
           >>= expireMulti "myhash" (Seconds 1.0)
        {-- val <- C.setKeyMulti "tt" "100" multi >>= C.setexKeyMulti "testing" "200" "1000" >>= C.incrMulti "testing" >>= C.getKeyMulti "tt" --}
        val <- execMulti multi
        checkValue val 1 "myvalue"
        checkValue val 3 "100"
        delay $ Milliseconds 1100.0
        expVal <- get cacheConn "myhash"
        case expVal of
             Right Nothing  -> pure unit
             Right (Just v) -> fail "Key has not expired"
             Left err       -> fail $ show err
  where
        checkValue res i value = case (flip index i <$> res) of
                                      Right (Just v) | v == value -> pure unit
                                      Right (Just v')             -> fail $ "Bad value: " <> v'
                                      Right Nothing               -> fail $ "Did not get value"
                                      Left err                    -> fail $ show err
