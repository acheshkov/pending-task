module RedisTask 
(
	getTasks
)
where

import Types
import Data.ByteString.Char8(pack,empty)
import Database.Redis.Redis



getTasks :: IO [Task]
getTasks = return $ replicate 20 testTask


saveKeyValue :: Key -> Value -> IO ()
saveKeyValue = undefined


testTask :: Task 
testTask = HTTPTask [] [] (pack "") (pack "http://api.draugiem.lv/json/") empty (pack "GET")
