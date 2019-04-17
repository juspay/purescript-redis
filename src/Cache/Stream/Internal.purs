module Cache.Stream.Internal where

import Cache.Types (CacheConn)
import Control.Promise (Promise)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, Fn7)
import Prelude (Unit)

foreign import xaddJ :: Fn4 CacheConn String String (Array String) (Promise String)
foreign import xdelJ :: Fn3 CacheConn String String (Promise Int)
foreign import xlenJ :: Fn2 CacheConn String (Promise Int)
-- The return value is of the form: [ [[entry_id, [key1, val1, key2, val2]], [entry_id, [...]]] ]
foreign import xrangeJ :: Fn5 CacheConn String String String Int (Promise (Array Foreign))
-- The return value is of the form: [ [[entry_id, [key1, val1, key2, val2]], [entry_id, [...]]] ]
foreign import xrevrangeJ :: Fn5 CacheConn String String String Int (Promise (Array Foreign))
-- The return value is of the form: [ [stream_name, [[entry_id, [key1, val1, key2, val2]], [entry_id, [...]]]], [stream_name, [...]] ]
foreign import xreadJ :: Fn4 CacheConn Int (Array String) (Array String) (Promise Foreign)
foreign import xtrimJ :: Fn5 CacheConn String String Boolean Int (Promise Int)

foreign import xgroupCreateJ :: Fn4 CacheConn String String String (Promise Unit)
foreign import xgroupDestroyJ :: Fn3 CacheConn String String (Promise Unit)
foreign import xgroupDelConsumerJ :: Fn4 CacheConn String String String (Promise Unit)
foreign import xgroupSetIdJ :: Fn4 CacheConn String String String (Promise Unit)
foreign import xreadGroupJ :: Fn7 CacheConn String String Int Boolean (Array String) (Array String) (Promise Foreign)
foreign import xackJ :: Fn4 CacheConn String String (Array String) (Promise Int)
foreign import xclaimJ :: Fn7 CacheConn String String String Int (Array String) Boolean (Promise (Array Foreign))
