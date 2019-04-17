module Test.Multi where

import Prelude

import Cache (CacheConn, SetOptions(..), get)
import Cache.Multi (delMulti, execMulti, expireMulti, getMulti, hgetMulti, hsetMulti, newMulti, setMulti)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Array (index)
import Data.Array.NonEmpty (singleton)
import Data.Either (Either(..))
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Global.Unsafe (unsafeStringify)
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
           >>= delMulti (singleton "mykey")
           >>= hsetMulti "myhash" "firstKey" "100"
           >>= hgetMulti "myhash" "firstKey"
           >>= expireMulti "myhash" (Seconds 1.0)
        {-- val <- C.setKeyMulti "tt" "100" multi >>= C.setexKeyMulti "testing" "200" "1000" >>= C.incrMulti "testing" >>= C.getKeyMulti "tt" --}
        val <- execMulti multi
        checkValue val 1 "myvalue"
        checkValue val 2 1
        checkValue val 4 "100"
        delay $ Milliseconds 1100.0
        expVal <- get cacheConn "myhash"
        case expVal of
             Right Nothing  -> pure unit
             Right (Just v) -> fail "Key has not expired"
             Left err       -> fail $ show err
  where
        checkValue :: forall a e. Eq a => Either Error (Array Foreign) -> Int -> a -> Aff e Unit
        checkValue res i value = case (flip index i <$> res) of
                                      Right (Just v) | unsafeFromForeign v == value -> pure unit
                                      Right (Just v')                               -> fail $ "Bad value: " <> unsafeStringify v'
                                      Right Nothing                                 -> fail $ "Did not get value"
                                      Left err                                      -> fail $ show err
