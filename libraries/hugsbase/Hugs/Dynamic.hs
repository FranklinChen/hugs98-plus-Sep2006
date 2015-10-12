module Hugs.Dynamic(module Data.Dynamic, coerceDynamic, runDyn) where

import Data.Dynamic

coerceDynamic :: Typeable a => Dynamic -> a
coerceDynamic d = fromDyn d def
  where def = error ("coerceDynamic: expecting " ++ show (toDyn def) ++
			" found " ++ show d)

runDyn :: Dynamic -> IO ()
runDyn = coerceDynamic
