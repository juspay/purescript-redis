module Cache.Stream
  ( EntryID(AutoID)
  , TrimStrategy(..)
  , newEntryId
  , xadd
  , xlen
  , xtrim
  ) where

import Cache.Types (CacheConn, CacheAff)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (Error)
import Control.MonadPlus ((>>=))
import Control.Promise (Promise, toAff)
import Data.Array (length, singleton, (!!), (:))
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, fromString, shl) as BigInt
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.Function.Uncurried (Fn2, Fn4, Fn5, runFn2, runFn4, runFn5)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (Pattern(..))
import Data.String.CodePoints (split)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, bind, pure, show, ($), (&&), (<), (<$>), (<*>), (<<<), (<>), (==), (>=))

-- Streams API

-- Validate a BigInt as being an unsigned 64-bit integer
filter64bit :: BigInt -> Maybe BigInt
filter64bit n = if n >= bigZero && n < uint64Max then Just n else Nothing
  where
        bigZero       = BigInt.fromInt 0
        uint64Max     = BigInt.shl (BigInt.fromInt 1) 64.0

data EntryID = EntryID BigInt BigInt
             | AutoID

newEntryId :: BigInt -> BigInt -> Maybe EntryID
newEntryId ms seq = EntryID <$> filter64bit ms <*> filter64bit seq

instance showEntryID :: Show EntryID where
  show :: EntryID -> String
  show (EntryID ms seq) = toString ms <> "-" <> toString seq
  show (AutoID        ) = "*"

fromString :: String -> Maybe EntryID
fromString s =
  let parts         = split (Pattern "-") s
      ms            = parts !! 0 >>= BigInt.fromString >>= filter64bit
      seq           = parts !! 1 >>= BigInt.fromString >>= filter64bit
  in
    if length parts == 2
      then EntryID <$> ms <*> seq
      else Nothing

foreign import xaddJ :: Fn4 CacheConn String String (Array String) (Promise String)

xadd :: forall e. CacheConn -> String -> EntryID -> Array (Tuple String String) -> CacheAff e (Either Error EntryID)
xadd cacheConn key entryId args = do
  res <- attempt <<< toAff $ runFn4 xaddJ cacheConn key (show entryId) $ foldMap tupleToArray args
  -- unsafePartial here as we expect redis to never give us an invalid entry ID
  pure $ unsafePartial $ fromJust <<< fromString <$> res
  where
        tupleToArray (Tuple a b) = a : singleton b

-- FIXME: this and all Ints should actually be 64-bit (instead of the current 32-bit)
foreign import xlenJ :: Fn2 CacheConn String (Promise Int)

-- Note that the API does not distinguish between a non-existing key and an
-- empty stream
xlen :: forall e. CacheConn -> String -> CacheAff e (Either Error Int)
xlen cacheConn key = attempt <<< toAff $ runFn2 xlenJ cacheConn key

data TrimStrategy = Maxlen

instance showTrimStrategy :: Show TrimStrategy where
  show Maxlen = "MAXLEN"

foreign import xtrimJ :: Fn5 CacheConn String String Boolean Int (Promise Int)

xtrim :: forall e. CacheConn -> String -> TrimStrategy -> Boolean -> Int -> CacheAff e (Either Error Int)
xtrim cacheConn key strategy approx len = attempt <<< toAff $ runFn5 xtrimJ cacheConn key (show strategy) approx len
