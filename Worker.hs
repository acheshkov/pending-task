module Main 
()
where

import Control.Concurrent
import Types
import RedisTask
import Network.HTTP
import Data.ByteString.Char8(pack,unpack)
import Log
import System( getArgs )

main :: IO ()
main = do 
	(threadCountArg:[]) <- getArgs
	print threadCountArg
	let theadCount = (read threadCountArg)
	sharedList <- newMVar []
	log <- createLog "log.log"
	th1 <- forkIO $ tryUpdateList sharedList
	thList <- mapM (\_ -> forkIO $ workThread sharedList log) [1..theadCount]
	simulateLoop $ [th1] ++ thList
	
simulateLoop :: [ThreadId] -> IO ()
simulateLoop thList = getLine >> mapM_ killThread thList 

{-|
	рабочий процесс,  
-}
workThread :: Loger lg => MVar [Task] -> lg -> IO ()
workThread sharedList lg = do
--	print "+"
	taskMaybe <- modifyMVar sharedList (\l-> return $ if (null l) then ([],Nothing) else (tail l, Just (head l)))
--	print "worker got task"
	case taskMaybe of 
		Nothing -> print "no task" -- >> workThread sharedList
		Just task -> do 
		--	print "task here, make request"
			request <- buildRequest task
			response <- simpleHTTP request >>= getResponseBody
--			print response
			threadDelay $ 10 ^ 6 * 2
			doLog lg (show response)
			workThread sharedList lg


buildRequest :: Task -> IO (Request String)
buildRequest (HTTPTask p g pd url key rqType) 
	| (unpack rqType == "GET")  =  return $ (getRequest  (unpack url))
	| (unpack rqType == "POST") =  return $ (postRequest (unpack url)) {rqBody = (unpack pd)}

--buildRequest task  = return $ (postRequest (unpack $ taskUrl task))  {rqBody = unpack (rawPostData task)}


{-|
	поток обновляющий очередь заданий
-}
tryUpdateList :: MVar [Task] -> IO ()
tryUpdateList sharedList = do
	print "*"
	tasks <- getTasks 
	print "got task"
	case tasks of 
		[]        -> print "no task" >> tryUpdateList sharedList 
		otherwise -> modifyMVar_ sharedList (\l -> return $ l ++  tasks) >> threadDelay (10^6 * 30) -- >> tryUpdateList sharedList  
