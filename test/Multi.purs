module Test.Multi where

import Prelude

import Cache (class CacheConn, SetOptions(..), get)
import Cache.Multi (delMulti, execMulti, expireMulti, getMulti, hgetMulti, hsetMulti, newMulti, setMulti, xaddMulti, xdelMulti, xlenMulti, xrangeMulti, xreadMulti, xrevrangeMulti, xtrimMulti)
import Cache.Stream (firstEntryId, newEntryId)
import Cache.Types (EntryID(..), Item, TrimStrategy(..))
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Array (index, unsafeIndex)
import Data.Array.NonEmpty (singleton)
import Data.Array.Partial (head)
import Data.BigInt (fromInt)
import Data.Either (Either(..), fromRight)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

testStream :: String
testStream = "mystream"

testId :: EntryID
testId = unsafePartial $ fromJust $ newEntryId (fromInt 9999999) (fromInt 0)

testItem :: Item
testItem = Tuple "mykey" "myvalue"

testItemArr :: Array String
testItemArr = ["mykey", "myvalue"]

multiTest :: forall a. CacheConn a => a -> Spec _ Unit
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

        multi <- unsafePartial $ liftEff $
          newMulti cacheConn
          >>= delMulti (singleton testStream)
          >>= xaddMulti testStream testId [testItem]
          >>= pure <<< fromRight
          >>= xlenMulti testStream
          >>= xrangeMulti testStream MinID MaxID (Just 10)
          >>= xrevrangeMulti testStream MinID MaxID (Just 10)
          >>= xreadMulti (Just 10) [Tuple testStream firstEntryId]
          >>= xtrimMulti testStream Maxlen false 0
          >>= xdelMulti testStream testId
        val <- execMulti multi
        checkValue val 1 (show testId)
        checkValue val 2 1
        checkValue (unsafePartial $ head <<< coerceXrange <<< forceIndex 3 <$> val) 1 testItemArr
        checkValue (unsafePartial $ head <<< coerceXrange <<< forceIndex 4 <$> val) 1 testItemArr
        checkValue (unsafePartial $ head <<< forceIndex 1 <<< head <<< coerceXread <<< forceIndex 5 <$> val) 1 testItemArr
        checkValue val 6 1
        checkValue val 7 0
  where
        checkValue :: forall a e. Eq a => Either Error (Array Foreign) -> Int -> a -> Aff e Unit
        checkValue res i value = case (flip index i <$> res) of
                                      Right (Just v) | unsafeFromForeign v == value -> pure unit
                                      Right (Just v')                               -> fail $ "Bad value: " <> unsafeStringify v'
                                      Right Nothing                                 -> fail $ "Did not get value"
                                      Left err                                      -> fail $ show err

        coerceXrange :: Foreign -> Array (Array Foreign)
        coerceXrange = unsafeFromForeign

        coerceXread :: Foreign -> Array (Array (Array (Array Foreign)))
        coerceXread = unsafeFromForeign

        forceIndex :: forall a. Int -> Array a -> a
        forceIndex arr i = unsafePartial $ unsafeIndex i arr
