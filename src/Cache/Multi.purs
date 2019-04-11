module Cache.Multi
  ( Multi
  , delMulti
  , execMulti
  , expireMulti
  , getMulti
  , hgetMulti
  , hsetMulti
  , incrMulti
  , lindexMulti
  , lpopMulti
  , lpushMulti
  , newMulti
  , publishMulti
  , rpopMulti
  , rpushMulti
  , setMulti
  , subscribeMulti
  ) where

import Cache.Types (CacheConn, SetOptions)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAff)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Either (Either)
import Data.Int (round)
import Data.Maybe (Maybe, maybe)
import Data.Time.Duration (Milliseconds(..))
import Prelude (show, ($), (<<<))

foreign import data Multi :: Type

foreign import newMultiJ :: CacheConn -> Multi

foreign import setMultiJ :: String -> String -> String -> String -> Multi -> Multi
foreign import getMultiJ ::  String -> Multi -> Multi
foreign import delMultiJ :: Array String -> Multi -> Multi
foreign import expireMultiJ :: String -> String -> Multi -> Multi
foreign import incrMultiJ ::  String -> Multi -> Multi
foreign import hsetMultiJ :: String -> String -> String -> Multi -> Multi
foreign import hgetMultiJ :: String -> String -> Multi -> Multi
foreign import publishMultiJ :: String -> String -> Multi -> Multi
foreign import subscribeMultiJ :: String -> Multi -> Multi
foreign import rpopMultiJ :: String -> Multi -> Multi
foreign import rpushMultiJ :: String -> String -> Multi -> Multi
foreign import lpopMultiJ :: String -> Multi -> Multi
foreign import lpushMultiJ :: String -> String -> Multi -> Multi
foreign import lindexMultiJ :: String -> Int -> Multi -> Multi
foreign import execMultiJ :: Multi -> Promise (Array String)

newMulti :: CacheConn -> Multi
newMulti = newMultiJ

execMulti :: forall e. Multi -> Aff e (Either Error (Array String))
execMulti = attempt <<< toAff <<< execMultiJ

setMulti :: String -> String -> Maybe Milliseconds -> SetOptions -> Multi -> Multi
setMulti key value mExp opts = setMultiJ key value (maybe "" msToString mExp) (show opts)
  where
        msToString (Milliseconds v) = show $ round v

getMulti :: String -> Multi -> Multi
getMulti val = getMultiJ val

delMulti :: NonEmptyArray String -> Multi -> Multi
delMulti keys = delMultiJ (toArray keys)

expireMulti :: String -> String -> Multi -> Multi
expireMulti key ttl = expireMultiJ key ttl

incrMulti :: String -> Multi -> Multi
incrMulti key = incrMultiJ key

hsetMulti :: String -> String -> String -> Multi -> Multi
hsetMulti key field val = hsetMultiJ key field val

hgetMulti :: String -> String -> Multi -> Multi
hgetMulti key field = hgetMultiJ key field

publishMulti :: String -> String -> Multi -> Multi
publishMulti channel message = publishMultiJ channel message

subscribeMulti :: String -> Multi -> Multi
subscribeMulti channel = subscribeMultiJ channel

rpopMulti :: String -> Multi -> Multi
rpopMulti listName = rpopMultiJ listName

rpushMulti :: String -> String -> Multi -> Multi
rpushMulti listName value = rpushMultiJ listName value

lpopMulti :: String -> Multi -> Multi
lpopMulti listName = lpopMultiJ listName

lpushMulti :: String -> String -> Multi -> Multi
lpushMulti listName value = lpushMultiJ listName value

lindexMulti :: String -> Int -> Multi -> Multi
lindexMulti listName index = lindexMultiJ listName index
