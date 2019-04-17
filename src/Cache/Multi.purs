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
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAff)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, runFn2, runFn3, runFn4, runFn5)
import Data.Int (round)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..), Seconds)
import Prelude (show, ($), (<<<))

foreign import data Multi :: Type

foreign import newMultiJ :: forall e. CacheConn -> Eff e Multi

foreign import setMultiJ :: forall e. Fn5 String String String String Multi (Eff e Multi)
foreign import getMultiJ :: forall e. Fn2 String Multi (Eff e Multi)
foreign import delMultiJ :: forall e. Fn2 (Array String) Multi (Eff e Multi)
foreign import expireMultiJ :: forall e. Fn3 String Int Multi (Eff e Multi)
foreign import incrMultiJ :: forall e. Fn2 String Multi (Eff e Multi)
foreign import hsetMultiJ :: forall e. Fn4 String String String Multi (Eff e Multi)
foreign import hgetMultiJ :: forall e. Fn3 String String Multi (Eff e Multi)
foreign import publishMultiJ :: forall e. Fn3 String String Multi (Eff e Multi)
foreign import subscribeMultiJ :: forall e. Fn2 String Multi (Eff e Multi)
foreign import rpopMultiJ :: forall e. Fn2 String Multi (Eff e Multi)
foreign import rpushMultiJ :: forall e. Fn3 String String Multi (Eff e Multi)
foreign import lpopMultiJ :: forall e. Fn2 String Multi (Eff e Multi)
foreign import lpushMultiJ :: forall e. Fn3 String String Multi (Eff e Multi)
foreign import lindexMultiJ :: forall e. Fn3 String Int Multi (Eff e Multi)
foreign import execMultiJ :: Multi -> Promise (Array Foreign)

newMulti :: forall e. CacheConn -> Eff e Multi
newMulti = newMultiJ

execMulti :: forall e. Multi -> Aff e (Either Error (Array Foreign))
execMulti = attempt <<< toAff <<< execMultiJ

setMulti :: forall e. String -> String -> Maybe Milliseconds -> SetOptions -> Multi -> Eff e Multi
setMulti key value mExp opts = runFn5 setMultiJ key value (maybe "" msToString mExp) (show opts)
  where
        msToString (Milliseconds v) = show $ round v

getMulti :: forall e. String -> Multi -> Eff e Multi
getMulti val = runFn2 getMultiJ val

delMulti :: forall e. NonEmptyArray String -> Multi -> Eff e Multi
delMulti keys = runFn2 delMultiJ (toArray keys)

expireMulti :: forall e. String -> Seconds -> Multi -> Eff e Multi
expireMulti key ttl = runFn3 expireMultiJ key (round <<< unwrap $ ttl)

incrMulti :: forall e. String -> Multi -> Eff e Multi
incrMulti key = runFn2 incrMultiJ key

hsetMulti :: forall e. String -> String -> String -> Multi -> Eff e Multi
hsetMulti key field val = runFn4 hsetMultiJ key field val

hgetMulti :: forall e. String -> String -> Multi -> Eff e Multi
hgetMulti key field = runFn3 hgetMultiJ key field

publishMulti :: forall e. String -> String -> Multi -> Eff e Multi
publishMulti channel message = runFn3 publishMultiJ channel message

subscribeMulti :: forall e. String -> Multi -> Eff e Multi
subscribeMulti channel = runFn2 subscribeMultiJ channel

rpopMulti :: forall e. String -> Multi -> Eff e Multi
rpopMulti listName = runFn2 rpopMultiJ listName

rpushMulti :: forall e. String -> String -> Multi -> Eff e Multi
rpushMulti listName value = runFn3 rpushMultiJ listName value

lpopMulti :: forall e. String -> Multi -> Eff e Multi
lpopMulti listName = runFn2 lpopMultiJ listName

lpushMulti :: forall e. String -> String -> Multi -> Eff e Multi
lpushMulti listName value = runFn3 lpushMultiJ listName value

lindexMulti :: forall e. String -> Int -> Multi -> Eff e Multi
lindexMulti listName index = runFn3 lindexMultiJ listName index
