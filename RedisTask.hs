module RedisTask 
(
	getTasks,
	testTask
)
where

import Types
import Data.ByteString.Char8(pack,empty)
import Database.Redis.Redis



getTasks :: Int -> IO [Task]
getTasks taskCount = return $ replicate taskCount testTask


saveKeyValue :: Key -> Value -> IO ()
saveKeyValue = undefined


testTask :: Task 
testTask = HTTPTask [] [] (pack "") (pack "http://api.draugiem.lv/json/") empty (pack "GET")
