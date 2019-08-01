module Cache.SortedSet
(
  zadd
, zrange
, zincrby
, zrem
, zpopmax
, zpopmin
, ReturnOptions(..)
, Score
, Member
, Key
, SortedSetItem
) where

import Prelude

import Cache.Internal (readStringMaybe)
import Cache.Stream (readItems)
import Cache.Types (class CacheConn, SetOptions, Item)
import Control.Promise (Promise, toAff)
import Data.Array (foldl)
import Data.Either (Either)
import Data.Function.Uncurried (Fn3, Fn4, Fn5, runFn3, runFn4, runFn5)
import Data.Maybe (Maybe)
import Data.Number (fromString)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, attempt)
import Foreign (Foreign)
import Foreign.Object (Object, empty, insert)

foreign import zaddJ :: forall a. Fn5 a String String String (Object String) (Promise Int)
foreign import zrangeJ :: forall a. Fn4 a String Int Int (Promise (Array Foreign))
foreign import zincrbyJ :: forall a. Fn4 a String Number String (Promise String)
foreign import zremJ :: forall a. Fn3 a String (Array String) (Promise Int)
foreign import zpopminJ :: forall a. Fn3 a String Int (Promise (Array Foreign))
foreign import zpopmaxJ :: forall a. Fn3 a String Int (Promise (Array Foreign))

data ReturnOptions = Default
                | Changed

instance showReturnOptions :: Show ReturnOptions where
  show Default  = ""
  show Changed = "CH"

type Score = Number
type Member = String
type Key = String
type SortedSetItem = Tuple Score Member

zadd :: forall a. CacheConn a => a -> Key -> SetOptions -> ReturnOptions -> Array SortedSetItem -> Aff (Either Error Int)
zadd cacheConn key options returnOptions values = attempt <<< toAff $ runFn5 zaddJ cacheConn key (show options) (show returnOptions) getValuesAsObj
  where
    getValuesAsObj = foldl (\acc (Tuple field val) -> insert val (show field) acc) empty values --making val as key of the object as member is unique and score can be repeated

zrange :: forall a. CacheConn a => a -> Key -> Int -> Int -> Aff (Either Error (Array (Maybe String)))
zrange cacheConn key start stop = attempt <<< map (map readStringMaybe) <<< toAff $ runFn4 zrangeJ cacheConn key start stop

zincrby :: forall a. CacheConn a => a -> Key -> Number -> Member -> Aff (Either Error (Maybe Number))
zincrby cacheConn key increment member = attempt <<< map fromString <<< toAff $ runFn4 zincrbyJ cacheConn key increment member

zrem :: forall a. CacheConn a => a -> Key -> Array Member -> Aff (Either Error Int)
zrem cacheConn key memberArr = attempt <<< toAff $ runFn3 zremJ cacheConn key memberArr

zpopmin :: forall a. CacheConn a => a -> Key -> Int -> Aff (Either Error (Array Item))
zpopmin cacheConn key memberArr = do
  res <- attempt <<< toAff $ runFn3 zpopminJ cacheConn key memberArr
  pure do
    response <- res
    readItems response

zpopmax :: forall a. CacheConn a => a -> Key -> Int -> Aff (Either Error (Array Item))
zpopmax cacheConn key memberArr = do
  res <- attempt <<< toAff $ runFn3 zpopmaxJ cacheConn key memberArr
  pure do
    response <- res
    readItems response