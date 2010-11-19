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

type Pid = Int
data Worker =  Worker Pid (MVar [Task])
type Dispetcher = MVar [Worker]

main :: IO ()
main = do 
	(threadCountArg:(taskCountArg:[])) <- getArgs
	print threadCountArg
	print taskCountArg
	let theadCount = (read threadCountArg)
	let taskCount = (read taskCountArg)
	sharedList <- newMVar []
	workers <- mapM  mkWorker [1 .. theadCount]
	threadList <- mapM (\w -> forkIO $ startWorker w) workers
	dispetcherThread <- forkIO $ startDispetcher workers taskCount
	simulateLoop $ [dispetcherThread] ++ threadList
	
simulateLoop :: [ThreadId] -> IO ()
simulateLoop thList = getLine >> mapM_ killThread thList 

{-|
	создать структуру для рабочего процесса,  
-}
mkWorker :: Pid -> IO Worker 
mkWorker pid = do 
	mvar <- newEmptyMVar
	return $ Worker pid mvar 

workerQueue :: Worker -> MVar [Task]
workerQueue (Worker pid queue) = queue

{-|
	распределить задания между рабочими процессами, простой вариант
-}
distributeTasks :: [Task] -> [Worker] -> Int -> IO ()
distributeTasks [] _ _ = return ()
distributeTasks (tasks) (w:ws) n = do 
	let (t,ts) = splitAt n tasks
	forkIO $ addTasksToWorker t w
	distributeTasks ts ws n

{-|
	добавить список задач конретному рабочему процессу 
-}

addTasksToWorker :: [Task] -> Worker -> IO ()
addTasksToWorker tasks (Worker pid queue) = do 
	print $ "add task to worker, pid = " ++ (show pid)
	isEmpty <- isEmptyMVar queue
	case isEmpty of
		True  -> putMVar queue tasks 
		False -> modifyMVar_ queue (\l -> return $ l ++  tasks)



startWorker :: Worker -> IO ()
startWorker w@(Worker pid queue) = do
	print $ "Worker start, wait task.. = " ++ (show pid)
	tasks <- takeMVar queue
	print $ "Worker got tasks, pid = " ++ (show pid)
	mapM  (\t -> forkIO $ processOneTask t) tasks
	startWorker w
	print $ show pid
	where 
		processOneTask :: Task -> IO ()
		processOneTask task = do
			request  <- buildRequest task
			response <- simpleHTTP request >>= getResponseBody
			print $  (show pid) ++ " :" ++ (show response)
			return ()


startDispetcher :: [Worker] -> Int ->  IO ()
startDispetcher workers  taskCount = do 
	tasks <- getTasks taskCount
	case tasks of 
		[] -> print "no task" >> startDispetcher workers taskCount
		otherwise -> do 
			let wCount  = fromIntegral (length workers) 
			    tCount  = fromIntegral (length tasks)
			    n  = ceiling $ (tCount/wCount)
			print $ "n = " ++ (show n)
			distributeTasks tasks workers n

buildRequest :: Task -> IO (Request String)
buildRequest (HTTPTask p g pd url key rqType) 
	| (unpack rqType == "GET")  =  return $ (getRequest  (unpack url))
	| (unpack rqType == "POST") =  return $ (postRequest (unpack url)) {rqBody = (unpack pd)}

