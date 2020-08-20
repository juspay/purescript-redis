module Cache.Stream.Internal where

import Cache.Types (Item)
import Data.Array (singleton)
import Data.Bifoldable (bifoldMap)
import Data.Foldable (foldMap)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, Fn6, Fn7)

-- These functions can take either a CacheConn or a Multi, and will return a
-- Promise with the actual value (in comment) or an Eff _ Multi respectively

foreign import xaddJ :: forall a b. Fn4 a String String (Array String) b -- String
foreign import xdelJ :: forall a b. Fn3 a String String b -- Int
foreign import xlenJ :: forall a b. Fn2 a String b -- Int
-- The return value is of the form: [ [[entry_id, [key1, val1, key2, val2]], [entry_id, [...]]] ]
foreign import xrangeJ :: forall a b. Fn5 a String String String Int b -- (Array Foreign)
-- The return value is of the form: [ [[entry_id, [key1, val1, key2, val2]], [entry_id, [...]]] ]
foreign import xrevrangeJ :: forall a b. Fn5 a String String String Int b -- (Array Foreign))
-- The return value is of the form: [ [stream_name, [[entry_id, [key1, val1, key2, val2]], [entry_id, [...]]]], [stream_name, [...]] ]
foreign import xreadJ :: forall a b. Fn4 a Int (Array String) (Array String) b -- Foreign
foreign import xtrimJ :: forall a b. Fn5 a String String Boolean Int b -- Int

foreign import xgroupCreateJ :: forall a b. Fn4 a String String String b -- Unit
foreign import xgroupDestroyJ :: forall a b. Fn3 a String String b -- Unit
foreign import xgroupDelConsumerJ :: forall a b. Fn4 a String String String b -- Unit
foreign import xgroupSetIdJ :: forall a b. Fn4 a String String String b -- Unit
foreign import xreadGroupJ :: forall a b. Fn7 a String String Int Boolean (Array String) (Array String) b -- Foreign
foreign import xackJ :: forall a b. Fn4 a String String (Array String) b -- Int
foreign import xclaimJ :: forall a b. Fn7 a String String String Int (Array String) Boolean b -- (Array Foreign)
foreign import xinfogroupsJ :: forall a b. Fn2 a String b -- Foreign
foreign import xpendingJ :: forall a b. Fn6 a String String String String Int b

itemsToArray :: Array Item -> Array String
itemsToArray = foldMap (bifoldMap singleton singleton)
